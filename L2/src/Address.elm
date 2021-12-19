module Address exposing (..)

type alias Person = {first : String, last :String}

type alias Address = {street : String, nr : Int, city : String, country : String}

formatAddress : Address -> String
formatAddress address = address.street ++ " " ++ String.fromInt( address.nr ) ++ ", " ++ address.city ++ ", " ++ address.country