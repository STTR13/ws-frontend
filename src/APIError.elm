module APIError exposing (..)

import Graphql.Http
import Graphql.Http.GraphqlError


type Error
    = Server { code : Int, message : String }
    | Http Graphql.Http.HttpError
    | Graphql (List Graphql.Http.GraphqlError.GraphqlError)


toText : Error -> String
toText error =
    case error of
        Server { message } ->
            message

        Graphql errList ->
            if errList |> List.map .message |> List.member "not found" then
                "Your email-password combination hasn't been found in our database.\nPlease make sure you haven't typed a typo"

            else
                "There is an issue in the program (graphql).\nPlease post an issue on github"

        Http httpErr ->
            case httpErr of
                Graphql.Http.Timeout ->
                    "The communication with the server timed out.\nPlease make sure your internet connection is working properly"

                Graphql.Http.NetworkError ->
                    "There was an issue when we tried reaching to the server.\nPlease make sure your internet connection is working properly"

                _ ->
                    "There is an issue in the program (http).\nPlease post an issue on github"
