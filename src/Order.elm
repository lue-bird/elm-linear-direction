module Order exposing
    ( Ordering
    , tie
    , by, reverse
    , onTieNext
    )

{-| Comparing 2 things

@docs Ordering

@docs tie


### alter

@docs by, reverse


## combine

@docs onTieNext


## primitive

  - [`Case`](Case)
  - [`Char.Order`](Char-Order)
  - [`Int.Order`](Int-Order)
  - [`Float.Order`](Float-Order)
  - [`String.Order`](String-Order)
  - [`List.Order`](List-Order)
  - [`Maybe.Order`](Maybe-Order)


## prior art

  - [`matthewsj/elm-ordering`](https://dark.elm.dmy.fr/packages/matthewsj/elm-ordering/latest/Ordering)
      - most complete
      - bad naming `byField`: `byField Tuple.first`, ... are also possible
      - clutter `isOrdered`, both `byField` & `byFieldWith`, ...
      - verbose `... |> breakTiesWith ...` chain instead of `onTieNext [ ... ]`
      - missing `list`, `maybe`, ... orderings
  - [`TSFoster/elm-compare`](https://dark.elm.dmy.fr/packages/TSFoster/elm-compare/latest/Compare)
      - API is a bit scuffed: multiple andThens will be nested instead of flat, ...
      - missing `list`, `maybe`, ... compare operations
  - [`rtfeldman/elm-sorter-experiment`](https://dark.elm.dmy.fr/packages/rtfeldman/elm-sorter-experiment/latest/Sort)
      - wrapped in an opaque `type` → more verbose
      - missing `list`, `maybe`, ... sorters
      - `Sort.Set` & `Sort.Dict` are very nice! You can even use `Sort.Set.empty (Sorter.custom Order.x)` if you wanted
  - [`nikita-volkov/`: `Typeclasses-Classes-Comparison`](https://package.elm-lang.org/packages/nikita-volkov/typeclasses/latest/Typeclasses-Classes-Comparison)
      - almost no API, _only_ `int`,`float`,`comparable` & `map`)
      - [`HashingContainers.HashSet`](https://package.elm-lang.org/packages/nikita-volkov/hashing-containers/2.1.0/HashingContainers-HashSet),
        [`HashingContainers.HashDict`](https://package.elm-lang.org/packages/nikita-volkov/hashing-containers/2.1.0/HashingContainers-HashDict)
        are nice!
  - ... know others? → PR

-}


{-| How 2 thing compare to each other.
An `Ordering` can be used directly to `sortWith`, make comparisons, ...

    cardsSort : List Card -> List Card
    cardsSort =
        List.sortWith cardOrder

    cardOrder : Ordering Card

-}
type alias Ordering orderable =
    orderable -> orderable -> Order


{-| Always `EQ` for any 2 things

    tie 5555 -12345 --> EQ

-}
tie : Ordering orderable_
tie =
    \_ _ -> EQ


{-| `Order` by a transformed value. Examples:


#### access a part

    import String.Order
    import Char.Order

    [ { name = "Bo" }, { name = "Amy" }, { name = "Cam" } ]
        |> List.sortWith
            (Order.by .name
                (String.Order.greaterEarlier (Char.Order.alphabetically Order.tie))
            )
    --> [ { name = "Amy" }, { name = "Bo" }, { name = "Cam" } ]

    {-| `Order` `Tuple.first`, then on tie `Tuple.second`

        Order.tuple ( Int.Order.increasing, Int.Order.increasing ) ( 0, 2 ) ( 0, -2 )
        --→ GT
    -}
    tuple : ( Ordering part0, Ordering part1 ) -> Ordering ( part0, part1 )
    tuple ( part0Order, part1Order ) =
        Order.onTieNext
            [ Order.by Tuple.first part0Order
            , Order.by Tuple.second part1Order
            ]


#### unwrap a type union constructor

    import Case
    import Char.Order
    import Order exposing (Ordering)
    import String.Order

    type User
        = User { name : String }

    userNameOrder : Ordering Username
    userNameOrder =
        Order.by (\(User user) -> user)
            (Order.by .name
                (String.Order.greaterEarlier (Char.Order.alphabetically Case.lowerUpper))
            )


#### rank a simple choice

    type Case
        = Lower
        | Upper

    {-| `'A' < 'a'`
    -}
    upperLower : Ordering Case
    upperLower =
        Order.by
            (\case_ ->
                case case_ of
                    Upper ->
                        0

                    Lower ->
                        1
            )
            Int.Order.increasing


#### rank a choice

    module Card exposing (Card(..), CardNormal, order)

    import Order exposing (Ordering)
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type Card
        = Normal CardNormal
        | Joker

    type alias CardNormal =
        RecordWithoutConstructorFunction
            { value : Value, suite : Suite }

    normalOrder : Ordering CardNormal
    normalOrder =
        Order.onTieNext
            [ Order.by .suite suiteOrder
            , Order.by .value valueOrder
            ]

    order : Ordering Card
    order =
        \card0 card1 ->
            case ( card0, card1 ) of
                -- match all variants with _values_
                ( Normal normal0, Normal normal1 ) ->
                    normalOrder normal0 normal1

                ( other0, other1 ) ->
                    -- sort others according to tag "rank"
                    Order.by
                        (\card ->
                            case card of
                                Normal _ ->
                                    0

                                Joker ->
                                    1
                        )
                        Int.Order.increasing
                        other0
                        other1

Use ↑ once your type union grows to have lots of variants
where exhaustive matching has n^2 branches

For simple type unions with only 2 variants:

    order : Ordering Card
    order =
        \card0 card1 ->
            case ( card0, card1 ) of
                -- match all variants with _values_
                ( Normal normal0, Normal normal1 ) ->
                    normalOrder normal0 normal1

                ( Normal _, Joker ) ->
                    GT

                ( Joker, Normal _ ) ->
                    LT

                ( Joker, Joker ) ->
                    EQ

Pretty neat stuff!

-}
by :
    (orderable -> orderableMapped)
    -> Ordering orderableMapped
    -> Ordering orderable
by map mappedOrder =
    \o0 o1 -> mappedOrder (o0 |> map) (o1 |> map)


{-| Prioritize the [`Ordering`](#Ordering) by one aspect
and break ties with the following

    import Order exposing (Ordering)
    import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)

    type alias Card =
        RecordWithoutConstructorFunction
            { suite : Suite, value : Value }

([`RecordWithoutConstructorFunction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/)
tricks elm into not creating a `Card` function)

    type Suite
        = Clubs
        | Hearts
        | Diamonds
        | Spades

    type Value
        = Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Jack
        | Queen
        | King
        | Ace

    cardOrder : Ordering Card
    cardOrder =
        Order.onTieNext
            [ Order.by .suite suiteOrder
            , Order.by .value valueOrder
            ]

    suiteOrder : Ordering Suite
    suiteOrder =
        Order.by
            (\suite ->
                case suite of
                    Clubs ->
                        0

                    Hearts ->
                        1

                    Diamonds ->
                        2

                    Spades ->
                        3
            )
            Int.Order.increasing

-}
onTieNext : List (Ordering orderable) -> Ordering orderable
onTieNext bys =
    case bys of
        [] ->
            tie

        orderingFirst :: orderingFollowing ->
            orderingFirst |> onTie (onTieNext orderingFollowing)


onTie : Ordering orderable -> (Ordering orderable -> Ordering orderable)
onTie orderingBreakingTie =
    \ordering ->
        \a0 a1 ->
            ordering a0 a1
                |> onEQ (\() -> orderingBreakingTie a0 a1)


onEQ : (() -> Order) -> (Order -> Order)
onEQ orderBreakingTie =
    \order ->
        case order of
            LT ->
                LT

            EQ ->
                orderBreakingTie ()

            GT ->
                GT


{-| `a < b  ⇆  a > b`

    import Char.Order
    import String.Order

    [ "b", "c", "a" ]
        |> List.sortWith
            (String.Order.greaterEarlier
                (Char.Order.alphabetically Order.tie)
            )
    --> [ "a", "b", "c" ]

    [ "b", "c", "a" ]
        |> List.sortWith
            (String.Order.greaterEarlier
                (Char.Order.alphabetically Order.tie)
                |> Order.reverse
            )
    --> [ "c", "b", "a" ]

-}
reverse : Ordering orderable -> Ordering orderable
reverse =
    \order ->
        \o0 o1 ->
            -- ↓ arguments flipped
            order o1 o0
