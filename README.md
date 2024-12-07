# CLI | nullstellensatz

> "The Hilbert's Nullstellensatz is a celebrated theorem which allow us to construct a 'dictionary' between Geometry (varieties) and Algebra (ideals)."
> - David Cox et al.

The nullstellensatz cli is a tool to construct a 'dictionary' between Combinatorics and Theory of Algorithms.

## Build

This project is based in [Clojure](https://clojure.org/guides/install_clojure). To generate the CLI and run benchmarks the next dependencies are needed:

- [Hyperfine](https://github.com/sharkdp/hyperfine): A command-line benchmarking tool.
- [Leiningen](https://codeberg.org/leiningen/leiningen): Project automation for Clojure.
- [Sdkman](https://sdkman.io/): Software development kit manager.
- [GraalVM](https://www.graalvm.org/downloads/): Advanced JDK with ahead-of-time native image compilation.
- [Babashka](https://github.com/babashka/babashka): Native Clojure interpreter for scripting with fast startup.

Finally, running the `bb build` command, this will generate the CLI `nulls`.

## Examples

``` sh
# Enumerates the quantity of subsets for the finite set [6]
$ ./nulls --enumerate-subset-object "{:n 6}" # => 64
# Generates the object (subset) 50 in the finite set [6]
$ ./nulls --generate-subset-object "{:n 6 :m 50}" # => (1 5 6)
```

``` sh
# Enumerates the quantity of k-combinations (with k = 3) for the finite set [6]
$ ./nulls --enumerate-combination-object "{:n 6 :k 3}" # => 20
# Generates the object (3-combination) 10 in the finite set [6]
$ ./nulls --generate-combination-object "{:n 6 :k 3 :m 10}" # => [3 4 5]
```

## Benchmarks

``` sh
# BENCHMARK | Enumerates the quantity of subsets for the finite sets [1], [2], ..., [10]
$ bb enumerate
# BENCHMARK | Generates the objects with index 0 (empty subsets) within the finite sets [1], [2], ..., [10]
$ bb generate
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
