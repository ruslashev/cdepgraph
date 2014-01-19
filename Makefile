default:
	ghc main.hs -O3 -o cdepgraph

debug:
	runhaskell main.hs link-to-dir

graph:
	runhaskell main.hs link-to-dir | neato -T png > out.png

open: graph
	ristretto out.png

