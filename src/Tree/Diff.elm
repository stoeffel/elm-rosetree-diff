module Tree.Diff exposing
    ( Tree(..), Edit(..)
    , diff
    )

{-| Diffing two `Tree`s.

@docs Tree, Edit
@docs diff

-}

import Tree


{-| A tree with annotated `Edit`s.
-}
type Tree a
    = Node a (List (Edit (Tree a)))


{-| Annotations for changes between the two `Tree`s.
-}
type Edit a
    = Insert a
    | Delete a
    | Copy a
    | Replace a a


type alias Equals a =
    a -> a -> Bool


{-| Function to diff two `Tree`s.


## Example

    import Tree exposing (Tree, singleton, tree)
    import Tree.Diff as Diff

    Diff.diff (==)
        (tree "root"
            [ tree "folder"
                [ singleton "foo"
                , singleton "bar"
                ]
            , singleton "yeah"
            ]
        )
        (tree "root"
            [ tree "folder"
                [ singleton "foo" ]
            , tree "folder2"
                [ singleton "nice" ]
            ]
        )

    --> Diff.Copy
    -->     (Diff.Node "root"
    -->         [ Diff.Copy
    -->             (Diff.Node "folder"
    -->                 [ Diff.Copy (Diff.Node "foo" [])
    -->                 , Diff.Delete (Diff.Node "bar" [])
    -->                 ]
    -->             )
    -->         , Diff.Replace
    -->             (Diff.Node "yeah" [])
    -->             (Diff.Node "folder2" [ Diff.Insert (Diff.Node "nice" []) ])
    -->         ]
    -->     )

-}
diff : Equals a -> Tree.Tree a -> Tree.Tree a -> Edit (Tree a)
diff equals x y =
    let
        labelX =
            Tree.label x

        childrenX =
            Tree.children x

        labelY =
            Tree.label y

        childrenY =
            Tree.children y
    in
    if equals labelX labelY then
        diffLists equals childrenX childrenY
            |> Node labelX
            |> Copy

    else
        Replace
            (Node labelX <| List.map (toTree Delete) childrenX)
            (Node labelY <| List.map (toTree Insert) childrenY)


diffLists : Equals a -> List (Tree.Tree a) -> List (Tree.Tree a) -> List (Edit (Tree a))
diffLists equals xs ys =
    case xs of
        [] ->
            List.map (toTree Insert) ys

        x :: xRest ->
            case ys of
                [] ->
                    List.map (toTree Delete) xs

                y :: yRest ->
                    diff equals x y
                        :: diffLists equals xRest yRest


toTree : (Tree a -> Edit (Tree a)) -> Tree.Tree a -> Edit (Tree a)
toTree toEdit =
    Tree.restructure identity (restructure toEdit)


restructure : (Tree a -> Edit (Tree a)) -> a -> List (Edit (Tree a)) -> Edit (Tree a)
restructure toEdit label children =
    case children of
        [] ->
            toEdit (Node label [])

        xs ->
            toEdit (Node label xs)
