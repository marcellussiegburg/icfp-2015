play_icfp2015: Main.hs Commands.hs Data.hs IO.hs Random.hs Solver.hs
	cabal update
	cabal install --bindir=$(CURDIR) icfp2015.cabal
