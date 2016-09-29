import CompileAssistant
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
conanO'Brien = "It's a-me, conan O'Brien!"
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs , odd x]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
answerTriangles = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a+b+c == 24, a^2 + b^2 == c^2 ]
