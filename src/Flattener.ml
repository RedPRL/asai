open Bwd
open Bwd.Infix

open Explication
open ExplicatorSigs

type 'tag block =
  { begin_line_num : int
  ; end_line_num : int
  ; markers : (Range.position * 'tag marker) list
  ; line_markers : (int * 'tag) list}

type 'tag t = (Range.source * 'tag block list) list

let dump_block dump_tag fmt {begin_line_num; end_line_num; markers; line_markers} : unit =
  Format.fprintf fmt
    begin
      "@[<1>{" ^^
      "begin_line_num=%d;@ " ^^
      "end_line_num=%d;@ " ^^
      "@[<2>markers=@ @[%a@]@];@ " ^^
      "@[<2>marked_lines=@,@[%a@]@]" ^^
      "}@]"
    end
    begin_line_num end_line_num
    (Utils.dump_list (Utils.dump_pair Range.dump_position (dump_marker dump_tag))) markers
    (Utils.dump_list (Utils.dump_pair Format.pp_print_int dump_tag)) line_markers

let dump dump_tag =
  Utils.dump_list @@ Utils.dump_pair Range.dump_source (Utils.dump_list (dump_block dump_tag))

module Make (Tag : Tag) =
struct
  type unflattened_block =
    { begin_line_num : int
    ; end_line_num : int
    ; ranges : (Range.t * Tag.t) bwd}

  let compare_position (p1 : Range.position) (p2 : Range.position) =
    Utils.compare_pair Int.compare Int.compare
      (p1.offset, p1.line_num) (p2.offset, p2.line_num)

  (* Stage 1: group ranges into blocks *)
  module Splitter :
  sig
    val partition : block_splitting_threshold:int -> (Range.t * Tag.t) list -> unflattened_block bwd
  end
  =
  struct
    (* Sort the ranges by their beginning positions;
       if equal sort them by their ending positions in reverse (larger ranges go first);
       if still equal, sort them by priority (important ones go first) *)
    let sort_tagged =
      let compare_range (s1 : Range.t) (s2 : Range.t) =
        Utils.compare_pair compare_position (Utils.compare_opposite compare_position)
          (Range.split s1) (Range.split s2)
      in
      let compare_range_tagged (sp1, t1) (sp2, t2) =
        Utils.compare_pair compare_range Int.compare
          (sp1, Tag.priority t1) (sp2, Tag.priority t2)
      in
      List.stable_sort compare_range_tagged

    let singleton ((range, _) as r) : unflattened_block =
      { begin_line_num = Range.begin_line_num range
      ; end_line_num = Range.end_line_num range
      ; ranges = Emp <: r
      }

    let partition_sorted ~block_splitting_threshold l : unflattened_block bwd =
      let rec go (blocks : unflattened_block bwd) block =
        function
        | [] -> blocks <: block
        | ((range, _) as r) :: rs ->
          if Range.end_line_num range - block.end_line_num > block_splitting_threshold then
            go (blocks <: block) (singleton r) rs
          else
            let end_line_num = Int.min block.end_line_num (Range.end_line_num range) in
            go blocks {block with end_line_num; ranges = block.ranges <: r} rs
      in
      match l with
      | [] -> Emp
      | r :: rs ->
        go Emp (singleton r) rs

    let partition ~block_splitting_threshold l =
      partition_sorted ~block_splitting_threshold (sort_tagged l)
  end

  (* Stage 2: flatten out ranges into tokens *)
  module BlockFlattener :
  sig
    val flatten : (Range.t * Tag.t) list -> (Range.position * Tag.t marker) list
  end
  =
  struct
    type t =
      { begins : (Range.position * Tag.t marker) bwd
      ; points : (Range.position * Tag.t marker) list
      ; ends : (Range.position * Tag.t marker) list
      }

    let add {begins; points; ends} (range, tag) =
      let b, e = Range.split range in
      if compare_position b e = 0 then
        {begins; points = (b, Point tag) :: points; ends}
      else
        {begins = begins <: (b, RangeBegin tag); points; ends = (e, RangeEnd tag) :: ends}

    let sort_cmd =
      let compare_cmd (p1, _) (p2, _) =
        compare_position p1 p2
      in
      List.stable_sort compare_cmd

    let merge_cmd {begins; points; ends} =
      (begins <@ points) @> ends

    let flatten l =
      sort_cmd @@ merge_cmd @@
      List.fold_left add {begins = Emp; points = []; ends = []} l
  end

  module FileFlattener :
  sig
    val flatten : block_splitting_threshold:int -> (Range.t * Tag.t) list -> Tag.t block list
  end
  =
  struct
    let compare_line_marker (i1, t1) (i2, t2) =
      Utils.compare_pair Int.compare Int.compare
        (i1, Tag.priority t1) (i2, Tag.priority t2)

    let flatten_block ({begin_line_num; end_line_num; ranges} : unflattened_block) =
      let ranges = Bwd.to_list ranges in
      { begin_line_num
      ; end_line_num
      ; markers = BlockFlattener.flatten ranges
      ; line_markers =
          List.stable_sort compare_line_marker @@
          List.map (fun (range, tag) -> Range.end_line_num range, tag) ranges
      }

    let flatten ~block_splitting_threshold rs =
      List.map flatten_block @@ Bwd.to_list @@ Splitter.partition ~block_splitting_threshold rs
  end

  module Files :
  sig
    val flatten : block_splitting_threshold:int -> (Range.t * Tag.t) list -> (Range.source * Tag.t block list) list
  end
  =
  struct
    module FileMap = Map.Make(struct
        type t = Range.source
        let compare = Stdlib.compare
      end)

    let add m ((range, _) as data) =
      m |>
      FileMap.update (Range.source range) @@ function
      | None -> Some (Emp <: data)
      | Some rs -> Some (rs <: data)

    let priority l : int = List.fold_left (fun p (_, tag) -> Int.min p (Tag.priority tag)) Int.max_int l

    let compare_part (source1, priority1, _) (source2, priority2, _) =
      Utils.compare_pair Int.compare (Option.compare String.compare)
        (priority1, Range.title source1) (priority2, Range.title source2)

    let flatten ~block_splitting_threshold rs =
      rs
      |> List.fold_left add FileMap.empty
      |> FileMap.bindings
      |> List.map
        (fun (src, rs) ->
           let rs = Bwd.to_list rs in
           (src, priority rs, FileFlattener.flatten ~block_splitting_threshold rs))
      |> List.stable_sort compare_part
      |> List.map (fun (src, _, part) -> src, part)
  end

  let flatten = Files.flatten
end
