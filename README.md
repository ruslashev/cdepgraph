# cdepgraph - C(++) dependency graph generator

## Usage

    $ cdepgraph <directory>

Start a scan for source files in specified directory.

The resulting GraphViz (`neato`) code is outputted to `stdout`, so the following way might be preffered:

    $ cdepgraph src/ | neato -T png > out.png

In future there will probably be options such as `-o <outfile>`, but that's for later.

## License
See `LICENSE` file.

