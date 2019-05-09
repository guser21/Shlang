all:
	happy -gca ParDeclaration.y
	alex -g LexDeclaration.x
	ghc -XBangPatterns --make -j  Init.hs -o shlang
	ghc --make -j TestDeclaration.hs -o TestDeclaration 
	-rm -f *.log *.aux *.hi *.o *.dvi

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocDeclaration.* LexDeclaration.* ParDeclaration.* LayoutDeclaration.* SkelDeclaration.* PrintDeclaration.* TestDeclaration.* AbsDeclaration.* TestDeclaration ErrM.* SharedString.* ComposOp.* declaration.dtd XMLDeclaration.* Makefile*
	
