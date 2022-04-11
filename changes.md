## 5.0.0 plans

  - switch to `bounded-nat` for index & length
  - in `Array.Linear`
      - remove `access` in favor of `at`
      - remove `replaceWith replacement`
        in favor of `alter (\_ -> replacement)`
  - in `List.Linear`
      - remove `access` in favor of `at`

# changelog

### 4.1.0

  - in `Array.Linear`
      - added `at`
      - deprecated `access` in favor of `at`
      - deprecated `replaceWith replacement`
        in favor of `alter (\_ -> replacement)`
  - in `List.Linear`
      - added `at`
      - deprecated `access` in favor of `at`

### 4.0.0

  - renamed `module LinearDirection` to `Linear`
      - changed
        ```elm
        type LinearDirection
            = FirstToLast
            | LastToFirst
        ```
        to
        ```elm
        type DirectionLinear
            = Up
            | Down
        ```
          - 👍 less specific → can be used for `Set`, `Dict`, relative location, ...
          - 👎 less clear in situations where it's from one end to the other
          - 👍 short, not verbose
      - removed
        ```elm
        toFirstToLast : LinearDirection -> { length : Int } -> Int -> Int
        ```
          - unnecessary `length` evaluations
          - `case direction of ...` can be understood more easily
          - not always applicable for amounts
      - added
        ```elm
        type ExpectedIndexInRange
            = ExpectedIndexInLength Int
        ```
      - added
        ```elm
        at :
            location
            -> structure
            -> { structure : structure, location : location }
        ```
  - in `Array.Linear`
      - changed `drop index direction` to `drop ( direction, index )`
      - changed `take index direction` to `take ( direction, index )`
      - changed `foldFrom init direction reduce`
        to `foldFrom ( init, direction, reduce )`
      - changed
        ```elm
        ... |> Array.Linear.at index direction
        : Maybe
        ```
        to
        ```elm
        ...
          |> Linear.at ( direction, index )
          |> Array.Linear.access
        : Result ExpectedIndexInRange
        ```
      - changed
        ```elm
        ... |> Array.Linear.insertAt index direction toInsert
        ```
        to
        ```elm
        ...
          |> Linear.at ( direction, index )
          |> Array.Linear.insert (\() -> toInsert)
        ```
      - changed
        ```elm
        squeezeInAt :
            Int
            -> LinearDirection
            -> Array element
            -> Array element
            -> Array element
        ```
        to
        ```elm
        squeezeIn :
            Array.Array element
            -> { structure : Array.Array element
               , location : ( Linear.DirectionLinear, Basics.Int )
               }
            -> Array.Array element
        ```
      - changed
        ```elm
        ... |> Array.Linear.removeAt index direction toInsert
        ```
        to
        ```elm
        ...
          |> Linear.at ( direction, index )
          |> Array.Linear.remove
        ```
       - changed
        ```elm
        padToLength :
            Int -> LinearDirection -> element -> Array element -> Array element
        ```
        to
        ```elm
        padTo :
            { lengthMinimum : Basics.Int
            , pad :
                  ( Linear.DirectionLinear, Basics.Int -> Array.Array element )
            }
            -> Array.Array element
            -> Array.Array element
        ```
      - changed
        ```elm
        toChunksOf :
            Int
            -> LinearDirection
            -> Array element
            -> { chunks : Array (Array element), remainder : Array element }
        ```
        to
        ```elm
        toChunks :
            { length : Basics.Int, remainder : Linear.DirectionLinear }
            -> Array.Array element
            -> { chunks : Array.Array (Array.Array element)
               , remainder : Array.Array element
               }
        ```
  - in `List.Linear`
      - changed `drop index direction` to `drop ( direction, index )`
      - changed `take index direction` to `take ( direction, index )`
      - changed `foldFrom init direction reduce`
        to `foldFrom ( init, direction, reduce )`
      - changed
        ```elm
        ... |> List.Linear.at index direction
        : Maybe
        ```
        to
        ```elm
        ...
          |> Linear.at ( direction, index )
          |> List.Linear.access
        : Result ExpectedIndexInRange
        ```
      - added `alter`
  - added `module Order`
  - added `module Set.Linear`
  - added `module Dict.Linear`

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
