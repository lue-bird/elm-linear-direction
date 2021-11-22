# changes

## plans for 3.0.0

- in `List.` & `Array.LinearDirection`:
    - change `.fold dir red init` to `.foldFrom init dir red`
    - rename `.group` to `toChunksOf`

- only in `List.LinearDirection`:
    - change `.takeFrom dir n` to `take n dir`
    - change `.dropFrom dir n` to `drop n dir`

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
