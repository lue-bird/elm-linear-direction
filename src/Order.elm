module Order exposing
    ( Ordering
    , tie
    , int, float
    , tuple, maybe, list
    , char, string
    , Case(..), upperLower, lowerUpper
    , by, downOnTie
    , reverse
    )

{-| Comparing 2 things.

@docs Ordering


## defaults

@docs tie
@docs int, float
@docs tuple, maybe, list
@docs char, string


### casing

@docs Case, upperLower, lowerUpper


## "transform"

@docs by, downOnTie


### alter

@docs reverse


## prior art

  - [`matthewsj/elm-ordering`](https://dark.elm.dmy.fr/packages/matthewsj/elm-ordering/latest/Ordering)
      - most complete
      - bad naming `byField`: `byField Tuple.first`, ... are also possible
      - clutter `isOrdered`, both `byField` & `byFieldWith`, ...
      - verbose `... |> breakTiesWith ...` chain instead of `downOnTie [ ... ]`
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
  - ... know others? → PR

-}


{-| An `Ordering` to `sortWith`, make comparisons, and so on:

    cardsSort : List Card -> List Card
    cardsSort =
        List.sortWith cardOrder

    cardOrder : Ordering Card

-}
type alias Ordering orderable =
    orderable -> orderable -> Order


{-| `Order` `Int`s.

    Order.int 40 2
    --> GT

-}
int : Ordering Int
int =
    compare


{-| `Order` `Char`s.

    Order.char { case_ = Order.upperLower } 'b' 'D'
    --> LT

    Order.char { case_ = Order.upperLower } 'l' 'L'
    --> GT

-}
char : { case_ : Ordering Case } -> Ordering Char
char order =
    by ( Char.toLower, compare )
        |> onTie
            (by
                ( casing
                , \casing0 casing1 ->
                    case Maybe.map2 Tuple.pair casing0 casing1 of
                        Nothing ->
                            EQ

                        Just ( case0, case1 ) ->
                            order.case_ case0 case1
                )
            )


{-| `Order` `Float`s.

    Order.float 0.1 -0.2
    --> GT

-}
float : Ordering Float
float =
    compare


{-| `'a' < 'A'`
-}
lowerUpper : Ordering Case
lowerUpper =
    by
        ( \case_ ->
            case case_ of
                Lower ->
                    0

                Upper ->
                    1
        , int
        )


{-| `'A' < 'a'`
-}
upperLower : Ordering Case
upperLower =
    by
        ( \case_ ->
            case case_ of
                Upper ->
                    0

                Lower ->
                    1
        , int
        )


{-| `Case` of a `Char`. [`Ordering`](#Ordering)s:

  - [`lowerUpper`](#lowerUpper)
  - [`upperLower`](#upperLower)
  - [`tie`](#tie)

-}
type Case
    = Lower
    | Upper


{-| Always `EQ` independent of the arguments.
-}
tie : Ordering orderable_
tie =
    \_ _ -> EQ


casing : Char -> Maybe Case
casing =
    \char_ ->
        if char_ |> Char.isLower then
            Lower |> Just

        else if char_ |> Char.isUpper then
            Upper |> Just

        else
            Nothing


{-| `Order` `String`s by `Char`s first to last, specifying the [`Case`](#Case)-[`Ordering`](#Ordering):

    Order.string { case_ = Order.lowerUpper }
        "hello, human!"
        "hello, Human"
    --> LT

For string-number chunked text,
you can use [`mcordova47/`: `NaturalOrdering.compare`](https://dark.elm.dmy.fr/packages/mcordova47/elm-natural-ordering/latest/NaturalOrdering#compare)

    NaturalOrdering.compare "abc2.tff" "abc10.tff"
    --→ LT

Personally, I'd just store the `String` as something like

    type TextChunked
        = TextChunked ( String, Maybe (List ( Int, String )) )

and order that
to avoid converting too often.

-}
string : { case_ : Ordering Case } -> Ordering String
string order =
    map2 String.uncons
        (maybe
            (\( char0, other0 ) ( char1, other1 ) ->
                char order char0 char1
                    |> onEQ (\() -> string order other0 other1)
            )
        )


{-| `Order` `Nothing` < `Just`

    Order.maybe Order.int (Just -99999) Nothing
    --> GT

-}
maybe : Ordering content -> Ordering (Maybe content)
maybe elementOrder =
    \maybe0 maybe1 ->
        case ( maybe0, maybe1 ) of
            ( Nothing, Nothing ) ->
                EQ

            ( Nothing, Just _ ) ->
                LT

            ( Just _, Nothing ) ->
                GT

            ( Just content0, Just content1 ) ->
                elementOrder content0 content1


{-| `Order` `Err` < `Ok`

**Should not be exposed**

-}
result :
    { error : Ordering error
    , ok : Ordering ok
    }
    -> Ordering (Result error ok)
result caseOrder =
    \result0 result1 ->
        case ( result0, result1 ) of
            ( Err error0, Err error1 ) ->
                caseOrder.error error0 error1

            ( Ok content0, Ok content1 ) ->
                caseOrder.ok content0 content1

            ( Err _, Ok _ ) ->
                LT

            ( Ok _, Err _ ) ->
                GT


{-| Order `List`s by elements first to last.

    Order.list Order.int
        [ 11, 22, 33, 188 ]
        [ 11, 22, 34 ]
    --> LT

-}
list : Ordering element -> Ordering (List element)
list elementOrder =
    map2 listUncons
        (result
            { error = tie
            , ok =
                \( head0, tail0 ) ( head1, tail1 ) ->
                    elementOrder head0 head1
                        |> onEQ (\() -> list elementOrder tail0 tail1)
            }
        )


{-| `Order` `Tuple.first`, then on tie `Tuple.second`.

    Order.tuple ( Order.int, Order.int ) ( 0, 2 ) ( 0, -2 )
    --> GT

Try implementing `tuple`
with [`downOnTie`](#downOnTie) and [`by`](#by)
if you want :)

-}
tuple : ( Ordering part0, Ordering part1 ) -> Ordering ( part0, part1 )
tuple partOrders =
    let
        ( part0Order, part1Order ) =
            partOrders
    in
    downOnTie
        [ by ( Tuple.first, part0Order )
        , by ( Tuple.second, part1Order )
        ]


{-| `Order` by a transformed value. Examples:


#### access fields in a record

    [ { name = "Bo" }, { name = "Amy" }, { name = "Cam" } ]
        |> List.sortWith
            (Order.by ( .name, Order.string { case_ = Order.tie } ))
    --> [ { name = "Amy" }, { name = "Bo" }, { name = "Cam" } ]


#### unwrap a type union constructor

    import Order exposing (Ordering)

    type User
        = User { name : String }

    userNameOrder : Ordering Username
    userNameOrder =
        Order.by
            ( \(User user) -> user
            , Order.by ( .name, Order.string { case = Order.lowerUpper } )
            )


#### rank an enum

    type Case
        = Lower
        | Upper

    {-| `'A' < 'a'`
    -}
    upperLower : Ordering Case
    upperLower =
        Order.by
            ( \case_ ->
                case case_ of
                    Upper ->
                        0

                    Lower ->
                        1
            , Order.int
            )


#### rank a union

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
        Order.downOnTie
            [ Order.by ( .suite, suiteOrder )
            , Order.by ( .value, valueOrder )
            ]

    order : Ordering Card
    order =
        \card0 card1 ->
            case ( card0, card1 ) of
                -- match all variants with _values_
                ( Normal normal0, Normal normal1 ) ->
                    normalOrder normal0 normal1

                _ ->
                    -- sort others according to tag "rank"
                    Order.by
                        ( \card ->
                            case card of
                                Normal _ ->
                                    0

                                Joker ->
                                    1
                        , Order.int
                        )
                        card0
                        card1

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
    ( orderable -> orderableMapped
    , Ordering orderableMapped
    )
    -> Ordering orderable
by ( map, orderMapped ) =
    \o0 o1 -> orderMapped (o0 |> map) (o1 |> map)


{-| Prioritize the [`Ordering`](#Ordering) by one aspect
and break ties with the next.

    import List.Extra as List
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
        Order.downOnTie
            [ Order.by ( .suite, suiteOrder )
            , Order.by ( .value, valueOrder )
            ]

    suiteOrder : Ordering Suite
    suiteOrder =
        Order.by
            ( \suite ->
                case suite of
                    Clubs ->
                        0

                    Hearts ->
                        1

                    Diamonds ->
                        2

                    Spades ->
                        3
            , Order.int
            )

-}
downOnTie : List (Ordering orderable) -> Ordering orderable
downOnTie bys =
    case bys of
        [] ->
            tie

        orderingFirst :: orderingFollowing ->
            orderingFirst |> onTie (downOnTie orderingFollowing)


onTie : Ordering orderable -> Ordering orderable -> Ordering orderable
onTie orderingBreakingTie =
    \ordering ->
        \a0 a1 ->
            ordering a0 a1
                |> onEQ (\() -> orderingBreakingTie a0 a1)


onEQ : (() -> Order) -> Order -> Order
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

    [ "b", "c", "a" ]
        |> List.sortWith
            (Order.string { case_ = Order.tie })
    --> [ "a", "b", "c" ]

    [ "b", "c", "a" ]
        |> List.sortWith
            (Order.string { case_ = Order.tie }
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



--


map2 : (a -> b) -> (b -> b -> c) -> a -> a -> c
map2 argumentMap transform2Mapped =
    \argument0 argument1 ->
        transform2Mapped (argumentMap argument0) (argumentMap argument1)


listUncons :
    List element
    ->
        Result
            { expectedListFilled : () }
            ( element, List element )
listUncons =
    \list_ ->
        case list_ of
            [] ->
                { expectedListFilled = () } |> Err

            head :: tail ->
                ( head, tail ) |> Ok
