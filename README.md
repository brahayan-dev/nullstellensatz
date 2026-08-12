# CLI | nullstellensatz

> "Hilbert's Nullstellensatz is a celebrated theorem that allows us to construct a 'dictionary' between Geometry (varieties) and Algebra (ideals)."
> -- David Cox et al.

The nullstellensatz CLI is a tool to construct a 'dictionary' between Combinatorics and Theory of Algorithms. It is part of my [master's thesis](https://repository.javeriana.edu.co/items/e4992668-7cda-411b-846a-5e977e866562) in mathematical science.

## Build

This project is based on [Clojure](https://clojure.org/guides/install_clojure). To generate the CLI and run benchmarks the following dependencies are required:

- [Hyperfine](https://github.com/sharkdp/hyperfine): A command-line benchmarking tool.
- [Leiningen](https://codeberg.org/leiningen/leiningen): Project automation for Clojure.
- [Sdkman](https://sdkman.io/): Software development kit manager.
- [GraalVM](https://www.graalvm.org/downloads/): Advanced JDK with ahead-of-time native image compilation.
- [Babashka](https://github.com/babashka/babashka): Native Clojure interpreter for scripting with fast startup.
- [Matplotlib](https://matplotlib.org/): Visualization with Python.

### Ubuntu

In Ubuntu, run the following commands to install the dependencies

```sh
sudo apt install python3-matplotlib leiningen hyperfine build-essential zlib1g-dev
curl -s "https://get.sdkman.io" | bash
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk install java 23.0.1-graal
```

Finally, running `bb build` generates the `nulls` CLI.

## Examples

```sh
# Count the number of subsets of the finite set [6]
./nulls --enumerate-subset-object "{:n 6}" # => 64
# Generate the object with index 50 (a subset) of the finite set [6]
./nulls --generate-subset-object "{:n 6 :m 50}" # => (1 5 6)
```

```sh
# Count the number of k-combinations (with k = 3) of the finite set [6]
./nulls --enumerate-combination-object "{:n 6 :k 3}" # => 20
# Generate the object with index 10 (a 3-combination) of the finite set [6]
./nulls --generate-combination-object "{:n 6 :k 3 :m 10}" # => [3 4 5]
```

```sh
# Count the number of set-partitions of the finite set [6]
./nulls --enumerate-set-partition-object "{:n 6}" # => 203
# Generate the object with index 100 (a set-partition) of the finite set [6]
./nulls --generate-set-partition-object "{:n 6 :m 100}" # => [[1 5] [2 3 4 6]]
```

```sh
# Count the number of catalan objects of the finite set [3]
./nulls --enumerate-catalan-family-object "{:n 3}" # => 5
# Generate the object with index 3 (a dyck path) of the finite set [3]
./nulls --generate-catalan-family-object "{:n 3 :m 3}" # => [[0 0] [1 1] [2 2] [3 1] [4 0] [5 1] [6 0]]
```

```sh
# Count the number of complete linked diagrams of the finite set [3]
./nulls --enumerate-complete-linked-diagram-object "{:n 3}" # => 15
# Generate the object with index 11 (a complete linked diagram) of the finite set [3]
./nulls --generate-complete-linked-diagram-object "{:n 3 :m 11}" # => #{#{0 1} #{3 5} #{4 2}}
```

```sh
# Count the number of irreducible linked diagrams of the finite set [5]
./nulls --enumerate-irreducible-linked-diagram-object "{:n 5}" # => 248
# Generate the object with index 200 (an irreducible linked diagram) of the finite set [5]
./nulls --generate-irreducible-linked-diagram-object "{:n 5 :m 200}" # => ([1 3] [2 8] [4 6] [5 9] [7 10])
```

```sh
# Count the number of labeled connected graphs of the finite set [5]
./nulls --enumerate-labeled-connected-graph-object "{:n 5}" # => 728
# Generate the object with index 700 (a labeled connected graph) of the finite set [5]
./nulls --generate-labeled-connected-graph-object "{:n 5 :m 700}" # => [[2 3] [1 3] [1 4] [1 5] [2 5] [3 5] [4 5]]
```

## Benchmarks

The available values for the `--object` parameter are: `subset`, `combination`, `set-partition`, `catalan`, `complete-linked-diagram`, `irreducible-linked-diagram`, and `labeled-connected-graph`. Other useful options are:

- `--runs`: number of times the `./nulls` CLI runs with fixed parameters. It acts as the sample size for the measurement (generally we use the mean).
- `--warmup`: number of runs used to identify variability and potential outliers in the environment.
- `--index`: this parameter is used in generation only (`bb generate`). It is useful to generate a specific object and works like `:m` in the `./nulls` CLI.

```sh
# BENCHMARK | Count the number of subsets of the finite sets [1], [2], ..., [100]
bb enumerate --from="1" --to="100" --object="subset"
# BENCHMARK | Generate the objects with index 0 (empty subsets) of the finite sets [1], [2], ..., [100]
bb generate --from="1" --to="100" --object="subset"
```

These commands will generate json files with an ID built from a timestamp and a [code](https://github.com/brahayan-dev/nullstellensatz/blob/main/script/plot.py#L15-L21) in the directory `/data`, e.g. `enumerate-1a100-2024-01-23-09-25-07.json` and `generate-1a100-2024-01-19-18-28-22.json` respectively. Finally, to render a plot from a given benchmark there is a Python script:

```sh
./script/plot.py ./data/enumerate-1a100-2024-01-23-09-25-07.json
```

## License

Copyright © 2026 Brahayan Xavier Suárez Ramírez

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
