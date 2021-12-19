module Card exposing(..)

type alias Date = { month :Int, year : Int}
type alias CardNumber = {higherPart: Int , lowerPart: Int}
type Issuer = Visa | Mastercard
type alias CreditCard = { issuer : Issuer, number : CardNumber, expirationDate: Date}

dateCard = Date 4 2023
cardNumber = CardNumber 245 289
card = CreditCard Visa cardNumber dateCard

validateCard : Date -> CreditCard -> Bool
validateCard date mycard =
    case (date.year < mycard.expirationDate.year) of
     (True) ->  True
     (False) -> if (date.year == mycard.expirationDate.year) then 
                        if(date.month < mycard.expirationDate.month) then True
                        else False
                else False

isDateAfter : Date -> Date -> Bool
isDateAfter d1 d2 =
        case (d1.year < d2.year) of
     (True) ->  True
     (False) -> if (d1.year == d2.year) then 
                        if(d1.month < d2.month) then True
                        else False
                else False
