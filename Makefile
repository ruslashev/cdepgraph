DEBUG_DIR = ~/Documents/supvime/

default:
	ghc --make cdepgraph.hs -O2

debug:
	runhaskell cdepgraph.hs $(DEBUG_DIR)

graph:
	runhaskell cdepgraph.hs $(DEBUG_DIR) | neato -T png > out.png

open: graph
	gpicview out.png

