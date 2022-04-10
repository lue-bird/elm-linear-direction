## 4.0.0 plans

  - switch order of `-at index direction` to `-at direction index`

# changelog

### 3.1.0

  - added `List.Linear.alterAt`
  - added `module Order`

## 3.0.0

  - replace module name prefix `.LinearDirection` with `.Linear`

  - for `List.` & `Array.Linear`
      - changed `fold dir red init` to `.foldFrom init dir red`
      - changed `group : ... -> { groups : ..., less : ... }`
        to `toChunksOf : ... -> { chunks : ..., remainder : ... }`
      - remove `order`

  - only in `List.Linear`:
      - change `takeFrom dir n` to `take n dir`
      - change `dropFrom dir n` to `drop n dir`

  - only in `Array.Linear`:
      - removed `concat`
      - changed `resize dir len val` to `padToLength len dir val`

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
