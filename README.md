# CLI | nullstellensatz

> "The Hilbert's Nullstellensatz is a celebrated theorem which allow us to construct a 'dictionary' between Geometry (varieties) and Algebra (ideals)."
> -- David Cox et al.

The nullstellensatz CLI is a tool to construct a 'dictionary' between Combinatorics and Theory of Algorithms.

## Build

This project is based in [Clojure](https://clojure.org/guides/install_clojure). To generate the CLI and run benchmarks the next dependencies are needed:

- [Hyperfine](https://github.com/sharkdp/hyperfine): A command-line benchmarking tool.
- [Leiningen](https://codeberg.org/leiningen/leiningen): Project automation for Clojure.
- [Sdkman](https://sdkman.io/): Software development kit manager.
- [GraalVM](https://www.graalvm.org/downloads/): Advanced JDK with ahead-of-time native image compilation.
- [Babashka](https://github.com/babashka/babashka): Native Clojure interpreter for scripting with fast startup.
- [Matplotlib](https://matplotlib.org/): Visualization with Python.

Finally, running the `bb build` command, this will generate the CLI `nulls`.

## Examples

``` sh
# Enumerates the quantity of subsets for the finite set [6]
./nulls --enumerate-subset-object "{:n 6}" # => 64
# Generates the object with index 50 (a subset) in the finite set [6]
./nulls --generate-subset-object "{:n 6 :m 50}" # => (1 5 6)
```

``` sh
# Enumerates the quantity of k-combinations (with k = 3) for the finite set [6]
./nulls --enumerate-combination-object "{:n 6 :k 3}" # => 20
# Generates the object with index 10 (a 3-combination) in the finite set [6]
./nulls --generate-combination-object "{:n 6 :k 3 :m 10}" # => [3 4 5]
```

``` sh
# Enumerates the quantity of set-partitions for the finite set [6]
./nulls --enumerate-set-partition-object "{:n 6}" # => 203
# Generates the object with index 100 (a set-partition) in the finite set [6]
./nulls --generate-set-partition-object "{:n 6 :m 100}" # => [[1 5] [2 3 4 6]]
```

``` sh
# Enumerates the quantity of catalan objects for the finite set [3]
./nulls --enumerate-catalan-family-object "{:n 3}" # => 5
# Generates the object with index 3 (a dyck path) in the finite set [3]
./nulls --generate-catalan-family-object "{:n 3 :m 3}" # => [[0 0] [1 1] [2 2] [3 1] [4 0] [5 1] [6 0]]
```

``` sh
# Enumerates the quantity of catalan objects for the finite set [3]
./nulls --enumerate-catalan-family-object "{:n 3}" # => 5
# Generates the object with index 3 (a dyck path) in the finite set [3]
./nulls --generate-catalan-family-object "{:n 3 :m 3}" # => [[0 0] [1 1] [2 2] [3 1] [4 0] [5 1] [6 0]]
```

``` sh
# Enumerates the quantity of catalan objects for the finite set [3]
./nulls --enumerate-catalan-family-object "{:n 3}" # => 5
# Generates the object with index 3 (a dyck path) in the finite set [3]
./nulls --generate-catalan-family-object "{:n 3 :m 3}" # => [[0 0] [1 1] [2 2] [3 1] [4 0] [5 1] [6 0]]
```

``` sh
# Enumerates the quantity of catalan objects for the finite set [3]
./nulls --enumerate-catalan-family-object "{:n 3}" # => 5
# Generates the object with index 3 (a dyck path) in the finite set [3]
./nulls --generate-catalan-family-object "{:n 3 :m 3}" # => [[0 0] [1 1] [2 2] [3 1] [4 0] [5 1] [6 0]]
```

## Benchmarks

The available values for the `--object` parameter are: `subset`, `combination`, `set-partition`, `catalan` `complete-linked-diagram`, `irreducible-linked-diagram` and `labeled-connected-graph`. There are other helpful commands:
- `--runs`: number of times that `./nulls` CLI runs with fixed parameters. It can be thought like the sample size to take the measure (generally we use the mean).
- `--warnup`: number of runs used to identify the variability and potential outlayers in the environment.
- `--index`: this parameter is used in generation only (`bb generate`). It is useful to generate a specific object and works like `:m` in `./nulls` CLI.

``` sh
# BENCHMARK | Enumerates the quantity of subsets for the finite sets [1], [2], ..., [100]
bb enumerate --from="1" --to="100" --object="subset"
# BENCHMARK | Generates the objects with index 0 (empty subsets) within the finite sets [1], [2], ..., [100]
bb generate --from="1" --to="100" --object="subset"
```

These commands will generate json files with an ID build by timestamp and a [code](https://github.com/brahayan-dev/nullstellensatz/blob/main/script/plot.py#L15-L21) in the directory `/data`, e.g. `enumerate-1a100-2024-01-23-09-25-07.json` and `generate-1a100-2024-01-19-18-28-22.json` respectively. Finally, to render a plot with a given benchmark there is a script in Python:

``` sh
./script/plot.py ./data/enumerate-1a100-2024-01-23-09-25-07.json
```

## License

Copyright © 2024 Brahayan Xavier Suárez Ramírez

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
