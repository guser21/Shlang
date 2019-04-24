all:
	happy -gca ParDeclaration.y
	alex -g LexDeclaration.x
	ghc --make TestDeclaration.hs -o TestDeclaration

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocDeclaration.* LexDeclaration.* ParDeclaration.* LayoutDeclaration.* SkelDeclaration.* PrintDeclaration.* TestDeclaration.* AbsDeclaration.* TestDeclaration ErrM.* SharedString.* ComposOp.* declaration.dtd XMLDeclaration.* Makefile*
	
