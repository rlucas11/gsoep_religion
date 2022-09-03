riclpmUni <- '
## Latent Variables

tr05 =~ a*tr051 + b*tr052 + c*tr053
tr09 =~ a*tr091 + b*tr092 + c*tr093
tr13 =~ a*tr131 + b*tr132 + c*tr133
tr17 =~ a*tr171 + b*tr172 + c*tr173

## Correlated residuals

tr051 ~~ tr091
tr051 ~~ tr131
tr051 ~~ tr171
tr091 ~~ tr131
tr091 ~~ tr171
tr131 ~~ tr171
tr052 ~~ tr092
tr052 ~~ tr132
tr052 ~~ tr172
tr092 ~~ tr132
tr092 ~~ tr172
tr132 ~~ tr172
tr053 ~~ tr093
tr053 ~~ tr133
tr053 ~~ tr173
tr093 ~~ tr133
tr093 ~~ tr173
tr133 ~~ tr173

## Random Intercepts
ri_tr =~ 1*tr05 + 1*tr09 + 1*tr13 + 1*tr17
ri_r =~ 1*relig05 + 1*relig09 + 1*relig13 + 1*relig17

## Within-person centered variables

wtr05 =~ 1*tr05
wtr09 =~ 1*tr09
wtr13 =~ 1*tr13
wtr17 =~ 1*tr17

wr05 =~ 1*relig05
wr09 =~ 1*relig09
wr13 =~ 1*relig13
wr17 =~ 1*relig17

## Stabilities

wtr09 ~ st*wtr05
wtr13 ~ st*wtr09
wtr17 ~ st*wtr13

wr09 ~ sr*wr05
wr13 ~ sr*wr09
wr17 ~ sr*wr13

## Cross-Lags

wtr09 ~ clr*wr05
wtr13 ~ clr*wr09
wtr17 ~ clr*wr13

wr09 ~ clt*wtr05
wr13 ~ clt*wtr09
wr17 ~ clt*wtr13

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
ri_tr ~~ ri_r
wtr05 ~~ wr05
wtr09 ~~ wr09
wtr13 ~~ wr13
wtr17 ~~ wr17
'
