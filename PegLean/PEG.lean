import PegLean.Parser
import Std

open Parser


-- Types

-- Terminal
inductive PEGTerm
| literal : String → PEGTerm
| char : Char → PEGTerm
| charClass : List (Char × Char) → PEGTerm
| any : PEGTerm
deriving Repr

-- Expression (right hand side of rule)
inductive PEGExpr
| term : PEGTerm → PEGExpr
| nonTerm : String → PEGExpr
| seq : List PEGExpr → PEGExpr
| or : List PEGExpr → PEGExpr
| many : PEGExpr → PEGExpr
| rep : PEGExpr → PEGExpr
| opt : PEGExpr → PEGExpr
| andPred : PEGExpr → PEGExpr
| notPred : PEGExpr → PEGExpr
deriving Repr

-- Rule Definition
inductive PEGRule
| rule : String → PEGExpr → PEGRule
deriving Repr

-- For parsing result
inductive AST
| empty : AST
| rule : String → AST → AST
| literal : String → AST
| char : Char → AST
| ident : String → AST
| seq : List AST → AST
| opt : Option AST → AST
deriving Inhabited, Repr

-- For generating recursions when compiling parser
abbrev RuleTable := Std.HashMap String (Parser AST)


-- Terminals

-- " ... "
def literal' : Parser PEGTerm := do
  let quote := '\"'
  char quote
  let s ← repeated (charBut quote)
  char quote
  return .literal (String.mk s)

-- 'c'
def char' : Parser PEGTerm := do
  let quote := '\''
  char quote
  let c ← charBut quote
  char quote
  return .char c

-- [a-z0-9_]
def charClass : Parser PEGTerm := do
  let char' := charIf λ c ↦ (c ≠ ']') ∧ (c ≠ '-')
  char '['
  let classes ← repeated (do
    let l ← char'
    let r' ← option (do
      char '-'
      char'
    )
    let r := r'.getD l
    return (l, r)
  )
  char ']'
  return .charClass classes

-- .
def anyChar' : Parser PEGTerm := do
  char '.'
  return .any


-- Expressions

-- Terminal as an expression
def terminal : Parser PEGExpr := do
  let term ← literal' <|> char' <|> charClass <|> anyChar'
  skipWS
  return .term term

-- Identifier
def nonTerminal : Parser PEGExpr := do
  let cs ← repeated alphabet
  skipWS
  return .nonTerm (String.mk cs)

-- ( ... )
def parentheses (self : Parser α) : Parser α := do
  char '('
  skipWS
  let r ← self
  char ')'
  skipWS
  return r

mutual

unsafe def atom := terminal <|> nonTerminal <|> parentheses expression

-- optSuffixes ← atom[*+?]?
unsafe def optSuffixes : Parser PEGExpr := do
  let r ← atom
  let r' ← optional (charIn ['*', '+', '?'])
  skipWS
  match r' with
  | none => return r
  | some '*' => return .many r
  | some '+' => return .rep r
  | some '?' => return .opt r
  | _ => failure

-- optPrefixed ← [&!]?optSuffixes
unsafe def optPrefixes : Parser PEGExpr := do
  let r ← optional (charIn ['&', '!'])
  skipWS
  let r' ← optSuffixes
  match r with
  | none => return r'
  | some '&' => return .andPred r'
  | some '!' => return .notPred r'
  | _ => failure

-- optSeq ← optPrefixes+
unsafe def optSeq : Parser PEGExpr := do
  let r ← optPrefixes
  let rs ← many optPrefixes
  if rs.isEmpty then
    return r
  else
    return .seq (r::rs)

-- optChoce ← optSeq ('/' optSeq)+
unsafe def optChoice : Parser PEGExpr := do
  let r ← optSeq
  let rs ← many (do
    char '/'
    skipWS
    optSeq
  )
  if rs.isEmpty then
    return r
  else
    return .or (r::rs)

-- Right hand side of a PEG rule
unsafe def expression := optChoice

end

-- Parse an rule definition
unsafe def rule : Parser PEGRule := do
  let ident ← repeated alphabet
  skipWS
  literal "<-"
  skipWS
  let expr ← expression
  skipWS
  return .rule (String.mk ident) expr

-- Parse sequence of rule definitions
unsafe def pegParser : Parser (List PEGRule) := do
  skipWSNL
  let rules ← many (do
    let r ← rule
    skipWSNL
    return r
  )
  return rules


-- Compile a parser out of PEG

namespace PEGTerm

-- Generate a parser from a terminal rule
def compile : PEGTerm → Parser AST
| .literal s => do
  Parser.literal s
  return (AST.literal s)
| .char c => do
  Parser.char c
  return (AST.char c)
| .charClass rs => AST.char <$> rs.foldl (λ acc (l, r) => acc <|> charRange l r) failure
| .any => AST.char <$> Parser.any

end PEGTerm


namespace PEGExpr

-- Combine monads
def sequence [Monad m] : List (m α) → m (List α)
| [] => pure []
| x::xs => do
  let r ← x
  let rs ← sequence xs
  return (r::rs)

-- Generate a parser using the rule table
partial def compile (rt : RuleTable) : PEGExpr → Parser AST
| .term t  => t.compile
| .nonTerm s => delay (λ _ => (rt.getD s undefined))
| .seq xs => AST.seq <$> sequence (xs.map (compile rt))
| .or xs => xs.map (compile rt) |>.foldr (. <|> .) failure
| .many x => AST.seq <$> Parser.many (x.compile rt)
| .rep x => AST.seq <$> repeated (x.compile rt)
| .opt x => AST.opt <$> option (x.compile rt)
| .andPred x => do
  let _ ← Parser.andPred (x.compile rt)
  return (AST.seq [])
| .notPred x => do
  let _ ← Parser.notPred (x.compile rt)
  return (AST.seq [])

end PEGExpr


namespace PEGRule

-- Get the name of a rule
def ident : PEGRule → String
| .rule ident' _ => ident'

-- Get the expression of a rule
def expr : PEGRule → PEGExpr
| .rule _ expr' => expr'

-- Generate a rule table
partial def generateTable (rs : List PEGRule) : RuleTable :=
  let empty := rs.foldl (λ acc r => acc.insert r.ident undefined) Std.HashMap.empty
  let rec table : RuleTable :=
    rs.foldl (λ acc r ↦
      let parser := delay (λ _ => AST.rule r.ident <$> r.expr.compile table)
      acc.insert r.ident parser
    ) empty
  table

partial def compile (self : List PEGRule) (ident : String): Parser AST :=
  let table := generateTable self
  table.getD ident undefined

end PEGRule


namespace AST

-- Flatten and remove empty branches
partial def flatten (ast : AST) : AST :=
  let rec flatten' : AST → Option AST
  | seq [] => none
  | seq xs =>
    match xs.filterMap flatten' with
    | [] => none
    | [x] => some x
    | xs => some (seq xs)
  | rule name body =>
    match flatten' body with
    | some b => some (rule name b)
    | none => none
  | opt a =>
    match a.bind flatten' with
    | some x => some (opt (some x))
    | none => some (opt none)
  | other => some other
  (flatten' ast).getD empty

end AST


unsafe def generateParser (peg : String) (entryPoint : String) :=
  match pegParser.run' peg with
  | .err msg _ => panic! (s! "Parsing rules failed; {msg}")
  | .ok rules _ => AST.flatten <$> PEGRule.compile rules entryPoint
