DEBUG_DIR = ~/Documents/2.0/supvime/

default:
	ghc --make main.hs -O3 -o cdepgraph

debug:
	runhaskell main.hs $(DEBUG_DIR)

graph:
	runhaskell main.hs $(DEBUG_DIR) | neato -T png > out.png

open: graph
	gpicview out.png

