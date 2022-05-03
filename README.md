# llvm-codegen

A Haskell library for generating LLVM code. Inspired by the `llvm-hs`,
`llvm-hs-pure`, `llvm-hs-combinators` and the `llvm-hs-pretty` libraries.

## Why another LLVM library?

- Support for the latest LLVM (!)
- Support for latest GHCs

## TODO

- [ ] ModuleBuilder / IRBuilder monads
- [ ] Add support for the following instructions:
  - add
  - mul
  - sub
  - udiv
  - and
  - alloca
  - load
  - store
  - gep
  - trunc
  - zext
  - ptrtoint
  - bitcast
  - icmp
  - br
  - phi
  - retVoid
  - call
  - ret
  - switch
  - select
  - condBr
  - ... (add as needed)
- [ ] Add pretty printer for generating LLVM IR
- [ ] Add combinators for commonly used control flow constructs
- [ ] Look into approach for supporting the C API
