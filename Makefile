default:
	runhaskell main.hs

graph:
	runhaskell main.hs | neato -T png > out.png

open: graph
	ristretto out.png

