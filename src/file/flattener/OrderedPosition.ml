module S = Asai.Span

type t = S.position

let compare p1 p2 = Int.compare p1.S.offset p2.S.offset
