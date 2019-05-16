all:
	happy -gca ParDeclaration.y
	alex -g LexDeclaration.x
	ghc --make  -itypechecker:evaluator Init.hs -o interpreter
	-rm -f *.log *.aux *.hi *.o *.dvi 
	-rm -f ./typechecker/*.log ./typechecker/*.aux ./typechecker/*.hi ./typechecker/*.o ./typechecker/*.dvi 
	-rm -f ./evaluator/*.log ./evaluator/*.aux ./evaluator/*.hi ./evaluator/*.o ./evaluator/*.dvi 

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocDeclaration.* LexDeclaration.* ParDeclaration.* LayoutDeclaration.* SkelDeclaration.* PrintDeclaration.* TestDeclaration.* AbsDeclaration.* TestDeclaration ErrM.* SharedString.* ComposOp.* declaration.dtd XMLDeclaration.* Makefile*
	
