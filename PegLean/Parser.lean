structure ParsingContext where
  val : String
  pos : String.Pos

inductive ParsingResult α
| ok : α → ParsingContext → ParsingResult α
| err : String → ParsingContext → ParsingResult α

structure Parser (α : Type) where
  run : ParsingContext → ParsingResult α


namespace ParsingContext

def new (s : String) : ParsingContext where
  val := s
  pos := 0

def readChar? (self : ParsingContext) : Option Char :=
  self.val.get? self.pos

def next (self : ParsingContext) : ParsingContext :=
  { self with pos := self.val.next self.pos }

end ParsingContext


namespace ParsingResult

instance [Repr α] : Repr (ParsingResult α) where
  reprPrec self n :=
  match self with
  | .ok result _ => s! "OK: {Repr.reprPrec result n}"
  | .err msg ctx => s! "Err at pos {ctx.pos}: {msg}"

end ParsingResult


namespace Parser

-- Make a new context and run
def run' (self : Parser α) (s : String) : ParsingResult α :=
  self.run (ParsingContext.new s)

-- Rewrite the error message
def withErrMsg (self : Parser α) (s : String) : Parser α where
  run ctx :=
    match self.run ctx with
    | .ok result ctx' => .ok result ctx'
    | .err _ ctx' => .err s ctx'


-- Functor Instance
private def map (f : α → β) (a : Parser α) : Parser β where
  run ctx :=
    match a.run ctx with
    | .ok result ctx' => .ok (f result) ctx'
    | .err msg ctx' => .err msg ctx'

instance : Functor Parser where
  map := map


-- Monad Instance (return, do, ...)
private def pure (a : α) : Parser α where
  run ctx := .ok a ctx

private def bind (x : Parser α) (f : α → Parser β) : Parser β where
  run ctx :=
    match x.run ctx with
    | .ok result ctx' => (f result).run ctx'
    | .err msg ctx' => .err msg ctx'

instance : Monad Parser where
  pure := pure
  bind := bind


-- Alternative Instance (A <|> B)
private def failure : Parser α where
  run ctx := .err "" ctx

private def orElse (x : Parser α) (f : Unit → Parser α) : Parser α where
  run ctx :=
    match x.run ctx with
    | .ok result ctx' => .ok result ctx'
    | .err _ ctx' => (f ()).run ctx'

instance : Alternative Parser where
  failure := failure
  orElse := orElse


-- Other Combinators

-- Match zero or more selves
partial def many (self : Parser α) : Parser (List α) :=
  (do
    let r ← self
    let rs ← many self
    return r::rs
  ) <|> pure []

-- Match one or more selves
partial def repeated (self : Parser α) : Parser (List α) := do
  let r ← self
  let rs ← many self
  return r::rs

-- Return some when matches and none otherwise
def option (self : Parser α) : Parser (Option α) where
  run ctx :=
    match self.run ctx with
    | .ok result ctx' => .ok (some result) ctx'
    | .err _ ctx' => .ok none ctx'

-- Peek and match without consuming
def andPred (self : Parser α) : Parser α where
  run ctx :=
    match self.run ctx with
    | .ok result _ => .ok result ctx
    | .err msg _ => .err msg ctx

-- Peek and fail when matching without consuming
def notPred (self : Parser α) : Parser Unit where
  run ctx :=
    match self.run ctx with
    | .ok _ _ => .err (s! "Unexpected input (notPred)") ctx
    | .err _ _ => .ok () ctx

-- Skip self
def skip (self : Parser α) : Parser Unit := do
  let _ ← many self


-- Basic Parsers

def charIf (f : Char → Bool) : Parser Char where
  run ctx :=
    match ctx.readChar? with
    | none => .err (s! "Unexpected end of input") ctx
    | some c =>
      if f c then
        .ok c ctx.next
      else
        .err (s! "Unexpected character {c}") ctx

def any := charIf λ _ ↦ True
def char (c : Char) := do let _ ← charIf λ c' ↦ c = c'
def charBut (c : Char) := charIf λ c' ↦ c ≠ c'
def charRange (l r : Char) := charIf λ c ↦ l.toNat ≤ c.toNat ∧ c.toNat ≤ r.toNat
def charIn (v : List Char) := charIf λ c ↦ v.contains c
def charNotIn (v : List Char) := charIf λ c ↦ ¬ v.contains c

def lowerCase := charRange 'a' 'z'
def upperCase := charRange 'A' 'Z'
def alphabet := lowerCase <|> upperCase
def digit := charRange '0' '9'

def space := char ' '
def tab := char '\t'
def newline := char '\n'
def carret := char '\r'

def skipWS := skip (space <|> tab)
def skipNL := skip (newline <|> carret)
def skipWSNL := skip (space <|> tab <|> newline <|> carret)

-- Match given string
def literal (s : String) : Parser Unit := do
  for c in s.toList do
    (char c).withErrMsg (s! "Unexpected literal (literal {s})")

-- Match digits and return Nat
def numeral : Parser Nat := do
  let fromDigit d := d.toNat - '0'.toNat
  let rec fromDigits : List Char → Nat
  | [] => 0
  | d::ds => (fromDigit d) * 10 ^ ds.length + fromDigits ds
  let digits ← (repeated digit).withErrMsg (s! "Unexpected input; expected numeral")
  return fromDigits digits

end Parser
