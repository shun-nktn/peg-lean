import PegLean.PEG

def rules := "
Value <- [0-9]+ / [a-zA-z]+ / '(' Expr ')'
Product <- Value (('*' / '/') Value)*
Sum <- Product (('+' / '-') Product)*
Expr <- Sum
"

def entryPoint := "Expr"

unsafe def ruleAST := pegParser.run! rules

#eval ruleAST

unsafe def parser := AST.flatten <$> PEGRule.compile ruleAST entryPoint

#eval parser.run! "r+10*(e-f)"
