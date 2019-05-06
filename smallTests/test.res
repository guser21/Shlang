Program 
[FnDef Int (Ident "main") [] (Block [Decl Int [Init (Ident "a") (ELitInt 2)],Decl Int [Init (Ident "b") (ELitInt 3)],Decl Int [Init (Ident "x") (EApp (Ident "foo") [])],Ret (EAdd (EVar (Ident "a")) Plus (EVar (Ident "b")))]),
FnDef Int (Ident "foo") [] (Block [Decl Int [Init (Ident "a") (ELitInt 3)],Decl Int [Init (Ident "b") (ELitInt 3)],Ret (EAdd (EVar (Ident "a")) Plus (EVar (Ident "b")))])]
