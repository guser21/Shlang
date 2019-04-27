all:
	happy -gca ParDeclaration.y
	alex -g LexDeclaration.x
	ghc --make Init.hs -o shlang 

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocDeclaration.* LexDeclaration.* ParDeclaration.* LayoutDeclaration.* SkelDeclaration.* PrintDeclaration.* TestDeclaration.* AbsDeclaration.* TestDeclaration ErrM.* SharedString.* ComposOp.* declaration.dtd XMLDeclaration.* Makefile*
	
