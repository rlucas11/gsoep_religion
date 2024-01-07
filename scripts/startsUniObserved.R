riclpmUniObserved <- '

## Random Intercepts
ri_tr =~ 1*tr05 + 1*tr09 + 1*tr13 + 1*tr17 
ri_r =~ 1*relig05 + 1*relig09 + 1*relig13 + 1*relig17 

## Within-person centered variables

wtr05 =~ 1*tr05
wtr09 =~ 1*tr09
wtr13 =~ 1*tr13
wtr17 =~ 1*tr17
wtr19 =~ 1*tr19

wr05 =~ 1*relig05
wr09 =~ 1*relig09
wr13 =~ 1*relig13
wr17 =~ 1*relig17
wr19 =~ 1*relig19

## Stabilities

wtr09 ~ st*wtr05
wtr13 ~ st*wtr09
wtr17 ~ st*wtr13
wtr19 ~ st2*wtr17

wr09 ~ sr*wr05
wr13 ~ sr*wr09
wr17 ~ sr*wr13
wr19 ~ sr2*wr17

## Cross-Lags

wtr09 ~ cl_r*wr05
wtr13 ~ cl_r*wr09
wtr17 ~ cl_r*wr13

wr09 ~ cl_t*wtr05
wr13 ~ cl_t*wtr09
wr17 ~ cl_t*wtr13

## Variances
ri_tr ~~ ri_tr
ri_r ~~ ri_r
wtr05 ~~ wtr05
wtr09 ~~ wtr09
wtr13 ~~ wtr13
wtr17 ~~ wtr17

wr05 ~~ wr05
wr09 ~~ wr09
wr13 ~~ wr13
wr17 ~~ wr17

tr05 ~~ 0*tr05
tr09 ~~ 0*tr09
tr13 ~~ 0*tr13
tr17 ~~ 0*tr17
relig05 ~~ 0*relig05
relig09 ~~ 0*relig09
relig13 ~~ 0*relig13
relig17 ~~ 0*relig17

## Covariances
ri_tr ~~ ri_r*ri_r
wtr05 ~~ r1_r*wr05
wtr09 ~~ r2_r*wr09
wtr13 ~~ r2_r*wr13
wtr17 ~~ r2_r*wr17

## Constrained to 0
ri_tr ~~ 0*wtr05
ri_tr ~~ 0*wr05
ri_r ~~ 0*wtr05
ri_r ~~ 0*wr05

'
