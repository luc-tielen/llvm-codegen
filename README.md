# llvm-codegen

A Haskell library for generating LLVM code. Inspired by the `llvm-hs`,
`llvm-hs-pure`, `llvm-hs-combinators` and the `llvm-hs-pretty` libraries.

**NOTE: WIP**

## Why another LLVM library?

- Support for the latest LLVM (!)
- Support for latest GHCs

## TODO

- [x] ModuleBuilder / IRBuilder monads
- [ ] MonadModuleBuilder, MonadIRBuilder typeclasses
- [ ] Add support for the following instructions:
  - [x] add
  - [x] mul
  - [x] sub
  - [x] udiv
  - [x] and
  - [x] alloca
  - [ ] load
  - [x] store
  - [x] gep
  - [x] trunc
  - [x] zext
  - [x] ptrtoint
  - [x] bitcast
  - [x] icmp
  - [x] br
  - [x] phi
  - [x] retVoid
  - [ ] call
  - [x] ret
  - [x] switch
  - [x] select
  - [x] condBr
  - ... (add as needed)
- [ ] Add combinators for commonly used control flow constructs
- [x] Add pretty printer for generating LLVM IR
- [ ] Look into approach for supporting the C API
- [ ] Support API with more compile time checks?
