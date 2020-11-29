
data Color = Red | Blue | Green

showColor :: Color -> String
showColor Red = "Red"
showColor Blue = "Blue"
showColor Green = "Green"

-- OUT(Red)
main :: String
main = showColor Red
