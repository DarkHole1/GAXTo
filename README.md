GAXTo
====

A simple [GAXT](https://esolangs.org/wiki/GAXT) interpreter written in ocaml. Supports:
- [x] Constants
- [x] Variables
- [x] Math
- [x] Logical
- [x] Print
- [x] Stack
- [ ] Flow
- [ ] Macro
- [ ] Loop
- [ ] Label
- [x] Service
- [ ] String

### Implementations notes
- Starts from CalcStack
- Current implementation puts $\rho$ on variable stack unchanged (it will be changed in future; however this is fine to spec)
- Nor operator converts ints to bools, instead of being bitwise. E.g. ```1 2 ` -> true true ` -> false -> 0```. Not sure, that's desired behavior. 
