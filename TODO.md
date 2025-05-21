# TODO

- [x] type check
  - [x] add u8, i8, u16, i16, u32, i32,
  - [x] handle arrays, slices, array pointer
  - [ ] add implicit casts when type checking, be able to make options
  - [ ] fix strings
- [x] add including and external declarations
  - [x] stop taking in symbols as arguments
  - [x] simplify the outside to just: source code -> flat code
- [x] add spans to everything needed to give good errors
  - [ ] don't stop collecting errors at first one
- [x] handle type unifying better, using ~~a state and~~ type variables
- [x] Don't have statements outside functions
  - [ ] split up program grammar to declarations in the top (including from other files) and then definitions afterwards
- [x] flatten
  - [ ] support arrays and structs (also slices)
  - [ ] fix strings
- [ ] add wrapping arithmetic operators
- [ ] codegen
- [ ] make pointers work
- [ ] add structs and enums
- [ ] don't use labels in flat structure, instead use blocks which each have their own state instead of functions.
      make sure epilogues get copied when blocks branch out so that stack allocs get freed in each block and make a clear mechanism for stackvars and temps carried over to new blocks
