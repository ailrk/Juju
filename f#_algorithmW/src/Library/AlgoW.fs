namespace AlgoW

open AlgoW
open System.Text.Json
open Syntax
open Parser

module AlgoW =
    let getJson value =
        let json = JsonSerializer.Serialize(value)
        value, json
