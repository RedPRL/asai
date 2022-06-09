%{
open Kai.Loc

open Syntax

let locate (start, stop) (value : 'a) : 'a Loc.t =
  {value; span = Span.create start stop}
%}

%token <string> ATOM
%token <int> NUMERAL
%token LPR RPR
%token CHECK
%token ARROW TIMES NAT
%token LAMBDA PAIR FST SND SUC NAT_REC
%token EOF

%start <Syntax.tm * Syntax.tp> defn

%%

%inline
locate(X):
  | e = X
    { locate $loc e }

defn:
  | LPR; CHECK; tm = term; tp = typ; RPR
    { tm, tp }

typ:
  | LPR; ARROW; a = typ; b = typ; RPR
    { Fun(a, b) }
  | LPR; TIMES; a = typ; b = typ; RPR
    { Tuple(a, b) }
  | NAT
    { Nat }

term:
  | tm = locate(term_)
    { tm }

term_:
  | nm = ATOM
    { Var nm }
  | LPR; LAMBDA; nm = ATOM; body = term; RPR
    { Lam(nm, body) }
  | LPR; PAIR; a = term; b = term; RPR
    { Pair(a, b) }
  | LPR; FST; a = term; RPR
    { Fst a }
  | LPR; SND; a = term; RPR
    { Snd a }
  | n = NUMERAL
    { Lit n }
  | LPR; SUC; a = term; RPR
    { Suc a }
  | LPR; NAT_REC; z = term; s = term; scrut = term; RPR
    { NatRec(z, s, scrut) }
