# min-caml-rs

<!---
[![Crates.io](https://img.shields.io/crates/v/min-caml-rs.svg)](https://crates.io/crates/min-caml-rs)
[![Docs.rs](https://docs.rs/min-caml-rs/badge.svg)](https://docs.rs/min-caml-rs)
[![codecov](https://codecov.io/gh/croys/min-caml-rs/branch/main/graph/badge.svg)](https://codecov.io/gh/croys/min-caml-rs)
--->
[![CI](https://github.com/croys/min-caml-rs/workflows/CI/badge.svg)](https://github.com/croys/min-caml-rs/actions)

<pre>
   _                                _
  (_)                              | |
  _ __ ___  _ _ __ ______ ___ __ _ _ __ ___ | |______ _ __ ___
  | '_ ` _ \| | '_ \______/ __/ _` | '_ ` _ \| |______| '__/ __|
  | | | | | | | | | |    | (_| (_| | | | | | | |      | |  \__ \
  |_| |_| |_|_|_| |_|     \___\__,_|_| |_| |_|_|      |_|  |___/

</pre>

![Screenshot](min-caml-rs-grab.png)

## About min-caml-rs

This is a port of min-caml from OCaml to Rust.

Min-caml is an educational compiler for a subset of OCaml, written in OCaml by Eijiro Sumii et al: https://github.com/esumii/min-caml


The intention is to provide LLVM and WebAssembly backends, not native
backends as per the original project.

This is largely being done as an exercise to learn Rust.
It will be a fairly direct port, with major module, type and function names
remaining the same, with a structure as close to the original OCaml code as
possible.

## Roadmap

* Lexer - done
* Parser - done
* Type inference/checking - done
* K-Normalization (conversion to SSA form) - done
* α-Conversion - done
* Optimizations
  * β-Reduction
  * let-Reduction
  * inline expansion
  * constant folding
  * unnecessary definition elimination
* Closure Conversion - done
* Virtual Machine Code generation - done
* LLVM Generation - mostly done
* System library
  * for reference interpreter - mostly done
  * for LLVM backend - mostly done
* Reference interpreter - done
* REPL
  * for reference interpreter
  * for LLVM
* Command line - mostly done

<!--
## Installation

### Cargo

* Install the rust toolchain in order to have cargo installed by following
  [this](https://www.rust-lang.org/tools/install) guide.
* run `cargo install min-caml-rs`
-->

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

See [CONTRIBUTING.md](CONTRIBUTING.md).
