# llvm-codegen

A Haskell library for generating LLVM code. Inspired by the `llvm-hs`,
`llvm-hs-pure`, `llvm-hs-combinators` and the `llvm-hs-pretty` libraries.

**NOTE: WIP**

## Why another LLVM library?

- Support for the latest LLVM (!)
- Support for latest GHCs

## TODO

- [ ] MonadModuleBuilder, MonadIRBuilder typeclasses
- [ ] Add support for remaining instructions as needed
- [ ] Add combinators for commonly used control flow constructs
- [ ] Look into approach for supporting the C API
- [ ] Support API with more compile time checks?
