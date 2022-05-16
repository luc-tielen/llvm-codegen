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
  - [ ] icmp
  - [x] br
  - [ ] phi
  - [x] retVoid
  - [ ] call
  - [x] ret
  - [ ] switch
  - [ ] select
  - [x] condBr
  - ... (add as needed)
- [ ] Add combinators for commonly used control flow constructs
- [x] Add pretty printer for generating LLVM IR
- [ ] Look into approach for supporting the C API
