open Bwd
open Bwd.Infix

open ExplicatorSigs

type 'tag block =
  { begin_line_num : int
  ; end_line_num : int
  ; tagged_positions : ('tag option * Range.position) list
  ; tagged_lines : ('tag * int) list}

type 'tag t = (Range.source * 'tag block list) list

let dump_block dump_tag fmt {begin_line_num; end_line_num; tagged_positions; tagged_lines} =
  Format.fprintf fmt {|@[<1>{begin_line_num=%d;@ end_line_num=%d;@ @[<2>tagged_positions=@ @[%a@]@];@ @[<2>tagged_lines=@,@[%a@]@]}@]|}
    begin_line_num end_line_num
    (Utils.dump_list (Utils.dump_pair (Utils.dump_option dump_tag) Range.dump_position)) tagged_positions
    (Utils.dump_list (Utils.dump_pair dump_tag Format.pp_print_int)) tagged_lines

let dump dump_tag =
  Utils.dump_list @@ Utils.dump_pair Range.dump_source (Utils.dump_list (dump_block dump_tag))

module Make (Tag : Tag) =
struct
  type unflattened_block =
    { begin_line_num : int
    ; end_line_num : int
    ; ranges : (Tag.t * Range.t) list}

  module Splitter :
  sig
    val partition : block_splitting_threshold:int -> (Tag.t * Range.t) list -> unflattened_block list
  end
  =
  struct
    let compare_range (s1 : Range.t) (s2 : Range.t) =
      Utils.compare_pair Int.compare Int.compare
        (Range.end_offset s1, Range.begin_offset s1)
        (Range.end_offset s2, Range.begin_offset s2)

    let compare_range_tagged (t1, sp1) (t2, sp2) =
      Utils.compare_pair compare_range Int.compare
        (sp1, Tag.priority t1)
        (sp2, Tag.priority t2)

    let sort_tagged = List.stable_sort compare_range_tagged

    let block_of_range ((_, sloc) as s) : unflattened_block =
      { begin_line_num = Range.begin_line_num sloc
      ; end_line_num = Range.end_line_num sloc
      ; ranges = [s]}

    let partition_sorted ~block_splitting_threshold l : unflattened_block list =
      let rec go ss block (blocks : unflattened_block list) =
        match ss with
        | Emp -> block :: blocks
        | Snoc (ss, ((_, sloc) as s)) ->
          if block.begin_line_num - Range.end_line_num sloc > block_splitting_threshold then
            go ss (block_of_range s) (block :: blocks)
          else
            let begin_line_num = Int.min block.begin_line_num (Range.begin_line_num sloc) in
            go ss {block with begin_line_num; ranges = s :: block.ranges} blocks
      in
      match Bwd.of_list l with
      | Emp -> []
      | Snoc (ss, s) ->
        go ss (block_of_range s) []

    let partition ~block_splitting_threshold l =
      partition_sorted ~block_splitting_threshold (sort_tagged l)
  end

  module BlockFlattener :
  sig
    val flatten : blend:(Tag.t -> Tag.t -> Tag.t) -> (Tag.t * Range.t) list -> (Tag.t option * Range.position) list
  end
  =
  struct
    type t = (Tag.t option * Range.position) bwd

    (* precondition: x1 < x2 and there are already points at x1 and x2 *)
    let impose ~blend xtag (x1 : Range.position) (x2 : Range.position) : t -> t =
      let blend_opt =
        function
        | None -> Some xtag
        | Some t -> Some (blend t xtag)
      in
      let[@tail_mod_cons] rec go2 : t -> t =
        function
        | Snoc (ps, (ptag, ploc)) when ploc.offset >= x1.offset ->
          Snoc (go2 ps, (blend_opt ptag, ploc))
        | ps -> ps
      in
      let[@tail_mod_cons] rec go1 : t -> t =
        function
        | Snoc (ps, ((_, ploc) as p)) when ploc.offset >= x2.offset ->
          Snoc (go1 ps, p)
        | ps -> go2 ps
      in
      go1

    let ensure_point (x : Range.position) =
      let[@tail_mod_cons] rec go : t -> t =
        function
        | Snoc (ps, ((_, ploc) as p)) when ploc.offset > x.offset ->
          Snoc (go ps, p)
        | Emp -> Emp <: (None, x)
        | Snoc (_, (ptag, p)) as ps ->
          if p.offset = x.offset then
            ps
          else
            ps <: (ptag, x)
      in
      go

    let add ~blend l (tag, value) =
      let x1, x2 = Range.split value in
      impose ~blend tag x1 x2 @@ ensure_point x2 @@ ensure_point x1 l

    let flatten ~blend l =
      Bwd.to_list @@ List.fold_left (add ~blend) Emp l
  end

  module File :
  sig
    val flatten : block_splitting_threshold:int -> blend:(Tag.t -> Tag.t -> Tag.t) -> (Tag.t * Range.t) list -> Tag.t block list
  end
  =
  struct
    let flatten_block ~blend ({begin_line_num; end_line_num; ranges} : unflattened_block) =
      { begin_line_num
      ; end_line_num
      ; tagged_positions = BlockFlattener.flatten ~blend ranges
      ; tagged_lines = List.map (fun (tag, value) -> tag, Range.end_line_num value) ranges
      }

    let flatten ~block_splitting_threshold ~blend rs =
      List.map (flatten_block ~blend) @@ Splitter.partition ~block_splitting_threshold rs
  end

  module Files :
  sig
    val flatten : block_splitting_threshold:int -> blend:(Tag.t -> Tag.t -> Tag.t) -> (Tag.t * Range.t) list -> (Range.source * Tag.t block list) list
  end
  =
  struct
    module FileMap = Map.Make(struct
        type t = Range.source
        let compare : t -> t -> int = Stdlib.compare
      end)

    let add m data =
      m |>
      FileMap.update (Range.source (snd data)) @@ function
      | None -> Some (Emp <: data)
      | Some rs -> Some (rs <: data)

    let priority l : int = List.fold_left (fun p (tag, _) -> Int.min p (Tag.priority tag)) Int.max_int l

    let compare_part (p1 : Range.source * int * Tag.t block list) (p2 : Range.source * int * Tag.t block list) =
      match p1, p2 with
      | (_, pri1, _), (_, pri2, _) when pri1 <> pri2 -> Int.compare pri1 pri2
      | (s1, _, _), (s2, _, _) -> Option.compare String.compare (Range.title s1) (Range.title s2)

    let flatten ~block_splitting_threshold ~blend rs =
      rs
      |> List.fold_left add FileMap.empty
      |> FileMap.bindings
      |> List.map (fun (src, rs) -> let rs = Bwd.to_list rs in src, priority rs, File.flatten ~block_splitting_threshold ~blend rs)
      |> List.filter (fun (_, _, l) -> l <> []) (* filter out sources with only empty ranges *)
      |> List.stable_sort compare_part
      |> List.map (fun (src, _, part) -> src, part)
  end

  let flatten = Files.flatten
end
