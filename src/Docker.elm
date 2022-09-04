module Docker exposing (..)

import Parser exposing ((|.), (|=))


type alias Image =
    { user : String, repo : String }


imageFromString : String -> Maybe Image
imageFromString imgStr =
    let
        string =
            Parser.chompWhile (\c -> Char.isLower c || Char.isDigit c || c == '-')
                |> Parser.getChompedString

        parser =
            Parser.succeed Image
                |. Parser.spaces
                |= string
                |. Parser.symbol "/"
                |= string
                |. Parser.spaces
                |. Parser.end
    in
    Parser.run parser imgStr
        |> Result.toMaybe
