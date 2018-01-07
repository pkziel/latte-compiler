all:
	# (cd lib/bnfc-176-source-position/source/; cabal install)
	make -C src
clean:
	make clean -C src
distclean:
	make distclean -C src