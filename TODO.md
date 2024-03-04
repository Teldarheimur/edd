# TODO

- [x] type check
  - [x] add u8, i8, u16, i16, u32, i32,
  - [ ] handle arrays, slices, array pointer and be able to make options
- [ ] add scopes to everything needed to give good errors
  - [ ] don't stop collecting errors at first one
- [ ] handle type unifying better, using a state and type variables
  - [ ] use i16 by default when compint is the final type
- [ ] Don't have statements outside functions, split up program grammar to declarations in the top (including from other files) and then definitions afterwards
- [ ] codegen
- [ ] make pointers work
- [ ] add structs and enums
