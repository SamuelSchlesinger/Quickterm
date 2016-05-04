import Quickterm
import qualified Data.HashMap as HM
import Text.Numeral.Roman
import System.Environment

qtRomanNumeral = Choice "roman" [
                                 qtRomanNumeralAdd,
                                 qtRomanNumeralSub,
                                 qtRomanNumeralExp
                                ] ""

qtRomanNumeralAdd = Command "add" (qtRomanNumeralOpf (+)) "<roman-num-n> <roman-num-m>"
qtRomanNumeralSub = Command "sub" (qtRomanNumeralOpf (-)) "<roman-num-n> <roman-num-m>"
qtRomanNumeralExp = Command "exp" (qtRomanNumeralOpf (^)) "<roman-num-n> <roman-num-m>"


qtRomanNumeralOpf :: (Integer -> Integer -> Integer) -> Args -> Options -> IO ()

qtRomanNumeralOpf op (str'n:str'm:_) _ = do
    case fromRoman str'n of
        Just n -> case fromRoman str'm of
            Just m -> let sum = op n m in if sum >= 0 then putStrLn (toRoman sum)
                                                     else putStrLn ("-" ++ (toRoman (-sum)))
            Nothing -> putStr (usage qtRomanNumeralAdd)
        Nothing -> putStr (usage qtRomanNumeralAdd)

main = do
    args <- getArgs
    quickrun args qtRomanNumeral
