# llvm-codegen

[![build](https://github.com/luc-tielen/llvm-codegen/actions/workflows/ci.yml/badge.svg)](https://github.com/luc-tielen/llvm-codegen/actions/workflows/ci.yml)

A Haskell library for generating LLVM code. Inspired by the `llvm-hs`,
`llvm-hs-pure`, `llvm-hs-combinators` and the `llvm-hs-pretty` libraries.

**NOTE:** WIP, but if you only need the provided instructions it's usable (and
tested). Used inside the [eclair compiler](https://github.com/luc-tielen/eclair-lang.git).

Note that it requires LLVM to be installed on your system and available on your
`$PATH`!

## Why another LLVM library?

- Support for the latest LLVM (!)
- Support for latest GHCs

## TODO

- [ ] Add support for remaining instructions as needed
- [ ] SIMD support, combinators
- [ ] Support API with more compile time checks?
- [ ] Documentation
