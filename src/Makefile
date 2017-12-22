all:
	bnfc Latte.cf 
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc Utils.hs
	ghc Compiler.hs
	ghc TypeChecker.hs
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* Latte.dtd 