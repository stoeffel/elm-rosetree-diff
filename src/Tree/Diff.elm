module Tree.Diff exposing
    ( Edit(..), Node(..), Children
    , diff
    )

{-| Diffing two `Tree`s.

@docs Edit, Node, Children
@docs diff

-}

import Tree exposing (Tree)


{-| A tree with annotated `Edit`s.
-}
type Edit a
    = Insert (Node a)
    | Delete (Node a)
    | Copy (Node a)
    | Replace (Node a) (Node a)


type Node a
    = Node a (Children a)


{-| Children of a node.
-}
type alias Children a =
    List (Edit a)


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
diff : Equals a -> Tree a -> Tree a -> Edit a
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


diffLists : Equals a -> List (Tree a) -> List (Tree a) -> List (Edit a)
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


toTree : (Node a -> Edit a) -> Tree a -> Edit a
toTree toEdit =
    Tree.restructure identity (restructure toEdit)


restructure : (Node a -> Edit a) -> a -> List (Edit a) -> Edit a
restructure toEdit label children =
    case children of
        [] ->
            toEdit (Node label [])

        xs ->
            toEdit (Node label xs)
