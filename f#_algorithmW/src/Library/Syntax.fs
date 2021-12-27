namespace AlgoW

module Syntax =
    type expr =
        | ExprUnit
        | Var of string
        | App of expr * expr
        | Lam of string * expr
        | Let of expr * expr

    type mono =
        | MonoUnit
        | MonoTVar of string
        | MonoTFun of mono * mono

    type poly =
        | PolyMono of mono
        | PolyQuan of string * poly
