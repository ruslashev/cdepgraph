default:
	runhaskell main.hs

graph:
	runhaskell main.hs | dot -T png > out.png

open: graph
	ristretto out.png

