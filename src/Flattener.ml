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
    ; ranges : (Range.t * Tag.t) list
    }

  let compare_position (p1 : Range.position) (p2 : Range.position) =
    Int.compare p1.offset p2.offset

  (* Stage 1: group ranges into blocks *)
  module RangePartitioner :
  sig
    val partition : block_splitting_threshold:int -> (Range.t * Tag.t) list -> unflattened_block list
  end
  =
  struct
    (* Sort the ranges by their ending positions;
       if equal sort them by their beginning positions in reverse (larger ranges go last);
       if still equal, sort them by priority (important ones go first) *)
    let sort_tagged =
      let compare_range (s1 : Range.t) (s2 : Range.t) =
        let b1, e1 = Range.split s1
        and b2, e2 = Range.split s2
        in
        Utils.compare_pair compare_position (Utils.compare_opposite compare_position)
          (e1, b1) (e2, b2)
      in
      let compare_range_tagged (sp1, t1) (sp2, t2) =
        Utils.compare_pair compare_range Int.compare
          (sp1, Tag.priority t1) (sp2, Tag.priority t2)
      in
      List.stable_sort compare_range_tagged

    let singleton ((range, _) as r) : unflattened_block =
      { begin_line_num = Range.begin_line_num range
      ; end_line_num = Range.end_line_num range
      ; ranges = [ r ]
      }

    let partition_sorted ~block_splitting_threshold l : unflattened_block list =
      let rec go rs block (blocks : unflattened_block list) =
        match rs with
        | Emp -> block :: blocks
        | Snoc (rs, ((range, _) as r)) ->
          if block.begin_line_num - Range.end_line_num range > block_splitting_threshold then
            go rs (singleton r) (block :: blocks)
          else
            let begin_line_num = Int.min block.begin_line_num (Range.begin_line_num range) in
            go rs {block with begin_line_num; ranges = r :: block.ranges} blocks
      in
      match l with
      | Emp -> []
      | Snoc (rs, r) ->
        go rs (singleton r) []

    let partition ~block_splitting_threshold l =
      partition_sorted ~block_splitting_threshold @@ Bwd.of_list @@ sort_tagged l
  end

  (* Stage 2: for each block, flatten out ranges into markers

     The code needs to handle several subtleties, using the XML-like notation to demonstrate:
     1. The ordering of markers and text strings should be ordered like this:
        <range1>...</range1><point/><range2>...</range2>
        Note that, in the middle, RangeEnd goes first, and then Point, and then RangeBegin.
     2. If the set of ranges is "well-scoped" (that is, a range is always completely included in,
        completely including, or being disjoint from another range), then matching beginning and
        ending markers will have the expected nested structures, like this:
        <range1><range2>...</range2><range3>...</range3></range1>
     3. For two ranges marking the same text with different priorities, the prioritized one goes inside.
        For two ranges with the same text and priority, the order of beginning markers will follow
        the order of the original input list. This is to reduce interruption of the prioritized highlighting.
        <low_pri1><low_pri2><high_pri1><high_pri2>...</high_pri2></high_pri1></low_pri2></low_pri1>
  *)
  module BlockFlattener :
  sig
    val flatten : (Range.t * Tag.t) list -> (Range.position * Tag.t marker) list
  end
  =
  struct
    type t =
      { begins : (Range.position * Tag.t marker) bwd
      ; points : (Range.position * Tag.t marker) bwd
      ; ends : (Range.position * Tag.t marker) list
      }

    let add (range, tag) {begins; points; ends} =
      let b, e = Range.split range in
      if b.offset = e.offset then
        {begins; points = points <: (b, Point tag); ends}
      else
        {begins = begins <: (b, RangeBegin tag); points; ends = (e, RangeEnd tag) :: ends}

    let sort_marker =
      let marker_order =
        function
        | RangeEnd _ -> -1
        | Point _ -> 0
        | RangeBegin _ -> 1
      in
      let compare_marker m1 m2 = Int.compare (marker_order m1) (marker_order m2) in
      List.stable_sort (Utils.compare_pair compare_position compare_marker)

    let merge_marker {begins; points; ends} =
      begins @> points @> ends

    let flatten l =
      sort_marker @@ merge_marker @@
      Bwd.fold_right add (Bwd.of_list l) {begins = Emp; points = Emp; ends = []}
  end

  module FileFlattener :
  sig
    val flatten : block_splitting_threshold:int -> (Range.t * Tag.t) list -> Tag.t block list
  end
  =
  struct
    let flatten_block ({begin_line_num; end_line_num; ranges} : unflattened_block) =
      let markers = BlockFlattener.flatten ranges in
      let line_markers =
        List.filter_map
          (function
            | (_, RangeBegin _) -> None
            | (p, RangeEnd tag) | (p, Point tag) -> Some (p.Range.line_num, tag))
          markers
      in
      { begin_line_num; end_line_num; markers; line_markers }

    let flatten ~block_splitting_threshold rs =
      List.map flatten_block @@ RangePartitioner.partition ~block_splitting_threshold rs
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
