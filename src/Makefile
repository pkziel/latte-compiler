all:
	../lib/bnfc-176-source-position/source/dist/build/bnfc/bnfc --functor Latte.cf 
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make TestLatte.hs -o TestLatte
	ghc Compiler.hs
	ghc LlvmGenerator.hs
	ghc TypeChecker.hs
	ghc Utils.hs
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* Latte.dtd 