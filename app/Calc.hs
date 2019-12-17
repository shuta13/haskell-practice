doublePlusTwo x = doubleX + 2
  where doubleX = x * 2

  -- 2.5
inc n = n + 1
double n = n * 2
square n = n * n

calcJudge n = if even n
              then n - 2
              else 3 * n + 1
