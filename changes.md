# changes log

## 10.0.0

  - `Array.Linear`
      - `glue` name → `attach`
  - `take ( dir, length )`, `drop ( dir, length )`
    → `take dir length`, `drop dir length`

## 9.0.0

  - `Linear.IndexIntOutOfRange` remove
      - results are now `Nothing`
  - `Array.Linear.elementRemove` name → `remove`
  - `module ....Order` move into `lue-bird/elm-keysset`

## 8.0.0

  - `Linear`
      - `directionToString` remove
      - `directionFuzz` add
  - `Order`
      - `downOnTie` → `onTieNext`
      - `by ( map, mappedOrder )` → `by map mappedOrder`
      - `tuple` remove
      - primitives move into respective `module X.Order` ↓
  - `Char.Order` add
      - `alphabetically` add
  - `Int.Order` add
      - `increasing` add
      - `decreasing` add
  - `Float.Order` add
      - `increasing` add
      - `decreasing` add
  - `String.Order` add
      - `greaterEarlier` add
  - `List.Maybe` add
      - `nothingJust` add
  - `List.Order` add
      - `greaterEarlier` add

#### 7.0.1

  - `Linear.Direction` documentation correct

## 7.0.0

  - `Array.Linear` actualize changes made to other `module`s in 6.0.0
    for `foldFrom`, `toChunksOf`

#### 6.0.1

  - readme correct, simplify

## 6.0.0

  - `foldFrom ( init, dir, reduce )` → `foldFrom init dir reduce`
  - `elementAlter ( location, alter )` → `elementAlter location alter`
  - `squeezeIn ( location, array )` → `squeezeIn location array`
  - `insert ( location, toInsert )` → `insert location toInsert`
  - `elementReplace ( location, replacement )` → `elementReplace location replacement`
  - `toChunks { length, remainder }` → `toChunksOf remainder length`
  - `Array.Linear.padTo { lengthMinimum, pad = ( direction, padding ) }`
    →
    `Array.Linear.padToLength direction padding lengthMinimum`
  - `module Linear`
      - ```elm
        ExpectedIndexInRange = ExpectedIndexForLength Int`
        →
        ```elm
        type IndexIntOutOfRange
            = IndexIntNegative
            | IndexIntBeyondElements
        ```
      - `DirectionLinear` name → `Direction`
  - `List.Linear.foldTraceFrom`, `.foldTrace` add

## 5.3.0

  - `Array.Linear`
      - glue` add

## 5.2.0

  - in `Array.Linear`
      - added `elementAlter`

## 5.1.0

  - in `Linear`
      - added `directionToString`

## 5.0.0

  - in `Linear`
      - removed `at`
  - in `Array.Linear`
      - renamed `at` to `element`
      - removed `access` in favor of `element`
      - changed
        ```elm
        remove :
            { structure : Array element
            , location : ( DirectionLinear, Int )
            }
            -> Array element
        ```
        to
        ```elm
        elementRemove :
            ( DirectionLinear, Int )
            -> Array element
            -> Array element
        ```
      - changed
        ```elm
        replaceWith :
            (() -> element)
            -> { structure : Array element
               , location : ( DirectionLinear, Int )
               }
            -> Array element
        ```
        to
        ```elm
        elementReplace :
            ( ( DirectionLinear, Int ), () -> element )
            -> Array element
            -> Array element
        ```
      - changed
        ```elm
        insert :
            (() -> element)
            -> { structure : Array element
               , location : ( DirectionLinear, Int )
               }
            -> Array element
        ```
        to
        ```elm
        insert :
            ( ( DirectionLinear, Int ), () -> element )
            -> Array element
            -> Array element
        ```
      - changed
        ```elm
        squeezeIn :
            (() -> Array element)
            -> { structure : Array element
               , location : ( DirectionLinear, Int )
               }
            -> Array element
        ```
        to
        ```elm
        squeezeIn :
            ( ( DirectionLinear, Int ), () -> Array element )
            -> Array element
            -> Array element
        ```
  - in `List.Linear`
      - renamed `at` to `element`
      - removed `access` in favor of `element`
      - changed
        ```elm
        alter :
            (element -> element)
            -> { structure : List element
               , location : ( DirectionLinear, Int )
               }
            -> List element
        ```
        to
        ```elm
        elementAlter :
            ( ( DirectionLinear, Int ), element -> element )
            -> List element
            -> List element
        ```

### 4.1.0

  - in `Array.Linear`
      - added `at`
      - deprecated `access` in favor of `at`
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
            Array element
            -> { structure : Array element
               , location : ( DirectionLinear, Int )
               }
            -> Array element
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
            { lengthMinimum : Int
            , pad :
                  ( DirectionLinear, Int -> Array element )
            }
            -> Array element
            -> Array element
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
            { length : Int, remainder : DirectionLinear }
            -> Array element
            -> { chunks : Array (Array element)
               , remainder : Array element
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
