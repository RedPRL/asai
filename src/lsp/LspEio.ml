open Eio
open Lsp.Import

module RPC = Jsonrpc

type io = {
  input : Buf_read.t;
  output : Eio_unix.sink_ty Eio.Resource.t;
}

(** See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#headerPart *)
module Header =
struct
  type t = {
    content_length : int;
    content_type : string
  }

  let empty = {
    content_length = -1;
    content_type = "application/vscode-jsonrpc; charset=utf-8"
  }

  let create ~(content_length : int) : t =
    { empty with content_length }

  let is_content_length key =
    String.equal (String.lowercase_ascii @@ String.trim key) "content-length"

  let is_content_type key =
    String.equal (String.lowercase_ascii @@ String.trim key) "content-type"

  (* NOTE: We should never really recieve an invalid header, as
     that would indicate a broken client implementation. Therefore,
     we just bail out when we see an invalid header, as there's
     no way we can really recover anyways. *)
  type header_error =
    | InvalidHeader of string
    | InvalidContentLength of string

  exception HeaderError of header_error

  (* If we do see any random header messages, we want to at least print out a decent error message. *)
  let () = Printexc.register_printer @@
    function
    | HeaderError (InvalidHeader err) -> Some (Format.asprintf "HeaderError: Invalid Header %s" err)
    | HeaderError (InvalidContentLength n) -> Some (Format.asprintf "HeaderError: Invalid Content Length '%s'" n)
    | _ -> None

  (* [TODO: Reed M, 09/06/2022] I could use some of the Buf_read parser module here, but this code works. *)
  let parse_header line headers =
    match String.split_on_char ~sep:':' @@ String.trim line with
    | [key; value] when is_content_length key ->
      let content_length =
        match int_of_string_opt (String.trim value) with
        | Some n -> n
        | None -> raise (HeaderError (InvalidContentLength value))
      in { headers with content_length }
    | [key; value] when is_content_type key ->
      let content_type = String.trim value in
      { headers with content_type }
    | [_; _] ->
      (* We skip any unknown headers. *)
      headers
    | _ ->
      raise (HeaderError (InvalidHeader line))

  (** Read the header section of an LSP message. *)
  let read io =
    let rec loop headers =
      match Buf_read.line io.input with
      | "" -> headers
      | line -> loop (parse_header line headers)
    in
    let headers = loop empty in
    if headers.content_length < 0 then
      raise (HeaderError (InvalidContentLength (string_of_int headers.content_length)))
    else
      headers

  (** Write out the header section of an LSP message. *)
  let write io headers =
    let header_str =
      Format.asprintf "Content-Type: %s\r\nContent-Length: %d\r\n\r\n"
        headers.content_type headers.content_length
    in
    Flow.copy_string header_str io.output
end

module Message =
struct
  let read io =
    try
      let header = Header.read io in
      let len = header.content_length in
      let json = Json.of_string @@ Buf_read.take len io.input in
      Some (RPC.Packet.t_of_yojson json)
    with
    | Sys_error _
    | End_of_file ->
      None

  let write io packet =
    let json = RPC.Packet.yojson_of_t packet in
    let data = Json.to_string json in
    let content_length = String.length data in
    let header = Header.create ~content_length in
    Header.write io header;
    Flow.copy_string data io.output
end

let init (env : Eio_unix.Stdenv.base) = {
  (* [TODO: Reed M, 09/06/2022] I should think about this buffer size... *)
  input = Buf_read.of_flow ~max_size:1_000_000 @@ Eio.Stdenv.stdin env;
  output = Eio.Stdenv.stdout env
}

let recv io =
  Message.read io

let send io packet =
  Message.write io packet
