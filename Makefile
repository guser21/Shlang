all:
	happy -gca ParDeclaration.y
	alex -g LexDeclaration.x
	ghc -XBangPatterns --make -j  -itypechecker:interpreter Init.hs -o shlang
	ghc --make -j TestDeclaration.hs -itypechecker:interpreter -o TestDeclaration 
	-rm -f *.log *.aux *.hi *.o *.dvi 
	-rm -f ./typechecker/*.log ./typechecker/*.aux ./typechecker/*.hi ./typechecker/*.o ./typechecker/*.dvi 
	-rm -f ./interpreter/*.log ./interpreter/*.aux ./interpreter/*.hi ./interpreter/*.o ./interpreter/*.dvi 

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocDeclaration.* LexDeclaration.* ParDeclaration.* LayoutDeclaration.* SkelDeclaration.* PrintDeclaration.* TestDeclaration.* AbsDeclaration.* TestDeclaration ErrM.* SharedString.* ComposOp.* declaration.dtd XMLDeclaration.* Makefile*
	
