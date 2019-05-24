doubleMe x = x + x

doubleUs x y = (doubleMe x) + (doubleMe y)

doubleSmallNumber x = if x > 100
                         then x
                         else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- function names cannot begin with an upper case letter
-- They can include "'" though
conanO'Brien = "Its a-me, Conan O'Brien!"

concatList = [1, 2] ++ [3, 4]
