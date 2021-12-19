module PracticeExercises exposing (..)

type Dice = One | Two | Three | Four | Five | Six

type alias DicePairAlias = (Dice, Dice)
type alias DicePairRec = { first : Dice, second : Dice}
type DicePairType = DicePair Dice Dice

luckyRoll : DicePairAlias -> String
luckyRoll dicePair =
    case dicePair of
        (Six, Six) -> "Very lucky"
        (Six, _) -> "Lucky"
        (_, Six) -> "Lucky"
        (_, _) -> "Meh"
