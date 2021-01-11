Was Babe Ruth’s 1921 season the best in baseball history?
================
Gabriel Costa and Kevin Cummiskey
12/4/2020

The following analysis uses the R Lahman package which imports Sean
Lahman’s database. The seasons data.frame contains a row for each player
and season from 1871 to 2019 with at least 502 plate appearences.

``` r
#calculate on base percentage, slugging, and OPS
seasons <- seasons %>% 
  mutate(OBP = (H + BB + HBP)/(AB + BB + SF + HBP),
         X1B = H - X2B - X3B - HR,
         SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB,
         OPS = OBP + SLG)
```

### The best seasons - OPS

The table below contains the highest OPS seasons in baseball history
(502 PA minimum). While Barry Bonds’ 2004 season had the highest OPS,
Bonds’ relatively few home runs that season make it tough to argue this
season is the best in baseball history. (On a side note, Bonds’ low home
run that season was not his fault - he nearly doubled the previous
record for intentional walks in a season. *it would be interesting to
take these intentional walks and see what his season would have looked
like if his performance on those at-bats was similar to this others.*)

| playerID  | nameFirst | nameLast | yearID |  AB |  PA |   OPS | HR |   G |   R |   H | X2B | X3B | RBI | SB | CS |  BB |  SO | IBB | HBP | SH | SF | GIDP | name          |   OBP | X1B |   SLG |
| :-------- | :-------- | :------- | -----: | --: | --: | ----: | -: | --: | --: | --: | --: | --: | --: | -: | -: | --: | --: | --: | --: | -: | -: | ---: | :------------ | ----: | --: | ----: |
| bondsba01 | Barry     | Bonds    |   2004 | 373 | 617 | 1.422 | 45 | 147 | 129 | 135 |  27 |   3 | 101 |  6 |  1 | 232 |  41 | 120 |   9 |  0 |  3 |    5 | Bonds 2004    | 0.609 |  60 | 0.812 |
| ruthba01  | Babe      | Ruth     |   1920 | 457 | 610 | 1.382 | 54 | 142 | 158 | 172 |  36 |   9 | 137 | 14 | 14 | 150 |  80 |   0 |   3 |  5 |  0 |    0 | Ruth 1920     | 0.533 |  73 | 0.849 |
| bondsba01 | Barry     | Bonds    |   2002 | 403 | 612 | 1.381 | 46 | 143 | 117 | 149 |  31 |   2 | 110 |  9 |  2 | 198 |  47 |  68 |   9 |  0 |  2 |    4 | Bonds 2002    | 0.582 |  70 | 0.799 |
| bondsba01 | Barry     | Bonds    |   2001 | 476 | 664 | 1.379 | 73 | 153 | 129 | 156 |  32 |   2 | 137 | 13 |  3 | 177 |  93 |  35 |   9 |  0 |  2 |    5 | Bonds 2001    | 0.515 |  49 | 0.863 |
| ruthba01  | Babe      | Ruth     |   1921 | 540 | 689 | 1.359 | 59 | 152 | 177 | 204 |  44 |  16 | 171 | 17 | 13 | 145 |  81 |   0 |   4 |  4 |  0 |    0 | Ruth 1921     | 0.512 |  85 | 0.846 |
| ruthba01  | Babe      | Ruth     |   1923 | 522 | 696 | 1.309 | 41 | 152 | 151 | 205 |  45 |  13 | 131 | 17 | 21 | 170 |  93 |   0 |   4 |  3 |  0 |    0 | Ruth 1923     | 0.545 | 106 | 0.764 |
| willite01 | Ted       | Williams |   1941 | 456 | 606 | 1.287 | 37 | 143 | 135 | 185 |  33 |   3 | 120 |  2 |  4 | 147 |  27 |   0 |   3 |  0 |  0 |   10 | Williams 1941 | 0.553 | 112 | 0.735 |
| bondsba01 | Barry     | Bonds    |   2003 | 390 | 550 | 1.278 | 45 | 130 | 111 | 133 |  22 |   1 |  90 |  7 |  0 | 148 |  58 |  61 |  10 |  0 |  2 |    7 | Bonds 2003    | 0.529 |  65 | 0.749 |
| ruthba01  | Babe      | Ruth     |   1927 | 540 | 677 | 1.258 | 60 | 151 | 158 | 192 |  29 |   8 | 164 |  7 |  6 | 137 |  89 |   0 |   0 | 14 |  0 |    0 | Ruth 1927     | 0.486 |  95 | 0.772 |
| willite01 | Ted       | Williams |   1957 | 420 | 546 | 1.257 | 38 | 132 |  96 | 163 |  28 |   1 |  87 |  0 |  1 | 119 |  43 |  33 |   5 |  0 |  2 |   11 | Williams 1957 | 0.526 |  96 | 0.731 |
| ruthba01  | Babe      | Ruth     |   1926 | 495 | 642 | 1.253 | 47 | 152 | 139 | 184 |  30 |   5 | 150 | 11 |  9 | 144 |  76 |   0 |   3 | 10 |  0 |    0 | Ruth 1926     | 0.516 | 102 | 0.737 |
| ruthba01  | Babe      | Ruth     |   1924 | 529 | 675 | 1.252 | 46 | 153 | 143 | 200 |  39 |   7 | 121 |  9 | 13 | 142 |  81 |   0 |   4 |  6 |  0 |    0 | Ruth 1924     | 0.513 | 108 | 0.739 |
| hornsro01 | Rogers    | Hornsby  |   1925 | 504 | 589 | 1.245 | 39 | 138 | 133 | 203 |  41 |  10 | 143 |  5 |  3 |  83 |  39 |   0 |   2 | 16 |  0 |    0 | Hornsby 1925  | 0.489 | 113 | 0.756 |
| gehrilo01 | Lou       | Gehrig   |   1927 | 584 | 696 | 1.240 | 47 | 155 | 149 | 218 |  52 |  18 | 175 | 10 |  8 | 109 |  84 |   0 |   3 | 21 |  0 |    0 | Gehrig 1927   | 0.474 | 101 | 0.765 |
| ruthba01  | Babe      | Ruth     |   1930 | 518 | 655 | 1.225 | 49 | 145 | 150 | 186 |  28 |   9 | 153 | 10 | 10 | 136 |  61 |   0 |   1 | 21 |  0 |    0 | Ruth 1930     | 0.493 | 100 | 0.732 |
| mcgwima01 | Mark      | McGwire  |   1998 | 509 | 681 | 1.222 | 70 | 155 | 130 | 152 |  21 |   0 | 147 |  1 |  0 | 162 | 155 |  28 |   6 |  0 |  4 |    8 | McGwire 1998  | 0.470 |  61 | 0.752 |
| foxxji01  | Jimmie    | Foxx     |   1932 | 585 | 701 | 1.218 | 58 | 154 | 151 | 213 |  33 |   9 | 169 |  3 |  7 | 116 |  96 |   0 |   0 |  0 |  0 |    0 | Foxx 1932     | 0.469 | 113 | 0.749 |
| thomafr04 | Frank     | Thomas   |   1994 | 399 | 517 | 1.217 | 38 | 113 | 106 | 141 |  34 |   1 | 101 |  2 |  3 | 109 |  61 |  12 |   2 |  0 |  7 |   15 | Thomas 1994   | 0.487 |  68 | 0.729 |
| hornsro01 | Rogers    | Hornsby  |   1924 | 536 | 627 | 1.203 | 25 | 143 | 121 | 227 |  43 |  14 |  94 |  5 | 12 |  89 |  32 |   0 |   2 | 13 |  0 |    0 | Hornsby 1924  | 0.507 | 145 | 0.696 |
| mcgwima01 | Mark      | McGwire  |   1996 | 423 | 548 | 1.198 | 52 | 130 | 104 | 132 |  21 |   0 | 113 |  0 |  0 | 116 | 112 |  16 |   8 |  0 |  1 |   14 | McGwire 1996  | 0.467 |  59 | 0.730 |

Highest OPS seasons in baseball history

### Comparing Ruth 1921 to Bonds 2004

The figure below is OPS vs. HRs for qualifying players (502 PAs) in 1921
and 2004. A few observations:

  - There was much less offense in 1921 than 2004.

  - Bonds’ OPS was exceptional. His HR total was not…he finished fourth
    that year.

  - Ruth was exceptional in both.

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Comparing standardized OPS and HR

Below, we calculate standardized OPS and HRs for player by season. The
standardized OPS is:

\[\frac{OPS - \overline{OPS}}{s_{OPS}}"\]

where \(OPS\) is the player’s OPS that season, \(\overline{OPS}\) is the
league average OPS that season, and \(s_{OPS}\) is the standard
deviation. This gives us a measure of the player’s performance relative
to other players that year.

``` r
#standardized OPS and HRs
seasons <- seasons %>% 
  group_by(yearID) %>% 
  mutate(OPS.mean = mean(OPS),
         OPS.stdev = sd(OPS),
         OPS.std = (OPS - OPS.mean)/OPS.stdev,
         HR.mean = mean(HR),
         HR.stdev = sd(HR),
         HR.std = (HR - HR.mean)/HR.stdev,
         HR_rate = AB/HR)
```

The figure below is standardized OPS and HRs for all qualifying seasons
(502 PAs) in baseball history.

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Questions for Father Costa:

1.  Why not Babe Ruth 1920?

### References

  - Friendly, M., Dalzell, C., Monkman, M., & Murphy, D. (2019). Sean
    Lahman’s Baseball Database. R package version 7.0-1.
