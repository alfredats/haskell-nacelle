-- Pre-question work (Reordering expressions)
z = 7
y = z + 8
x = y ^ 2
waxOn = x * 5 --waxOn = 1125

-- 1)   The expressions are all valid. The first two yield the same
--      values, but the last two would not. Ordering matters for the
--      subtraction operator

-- 2)
triple x = x * 3

-- 3)   triple waxOn should yield 3375. Checking this in ghci
--      verifies that it is indeed the case.

-- 4)   
waxOn2 = x2 * 5
    where x2 = y2 ^ 2
          y2 = z2 + 8
          z2 = 7             

-- 5)   triple waxOn2 yields the same value as triple waxOn

-- 6)
waxOff x = triple x
