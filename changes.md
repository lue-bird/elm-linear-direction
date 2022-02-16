## 3.0.0 plans

- rename ` module List.`, `Array.LinearDirection` to `.Linear`
    - change `.fold dir red init` to `.foldFrom init dir red`
    - rename `.group` to `toChunksOf`
    - remove `order`

- only in `List.Linear`:
    - change `.takeFrom dir n` to `take n dir`
    - change `.dropFrom dir n` to `drop n dir`

- only in `Array.Linear`:
    - remove `concat`

# changelog

### 2.3.0

- added `List.LinearDirection.at`
- corrected fold examples

### 2.2.1

- corrected `LinearDirection.Array` to `Array.LinearDirection` in readme

### 2.2.0

- added `takeFrom` & `dropFrom` in `List.LinearDirection`

### 2.1.0

- added `resize`

## 2.0.0

- added `List.LinearDirection`
- renamed `LinearDirection.Array` to `Array.LinearDirection`
- added `order` for `List` & `Array`
- added `Array.LinearDirection.concat`
- changed argument order for `LinearDirection.toFirstToLast`
- changed readme: examples & the why
