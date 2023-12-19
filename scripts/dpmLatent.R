dpmLatent <- '
## Latent Variables

tr05 =~ a*tr051 + b*tr052 + c*tr053
tr09 =~ a*tr091 + b*tr092 + c*tr093
tr13 =~ a*tr131 + b*tr132 + c*tr133
tr17 =~ a*tr171 + b*tr172 + c*tr173

## Random Intercept
tr =~ 1*tr09 + 1*tr13 + 1*tr17
r =~ 1*r09 + 1*r13 + 1*r17

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


## Stabilities

tr09 ~ st*tr05
tr13 ~ st*tr09
tr17 ~ st*tr13

r09 ~ sr*r05
r13 ~ sr*r09
r17 ~ sr*r13

## Cross-Lags

tr09 ~ cl_r*r05
tr13 ~ cl_r*r09
tr17 ~ cl_r*r13

r09 ~ cl_t*tr05
r13 ~ cl_t*tr09
r17 ~ cl_t*tr13

## Variances
tr05 ~~ tr05
tr09 ~~ tr09
tr13 ~~ tr13
tr17 ~~ tr17
tr ~~ tr


r05 ~~ r05
r09 ~~ r09
r13 ~~ r13
r17 ~~ r17


## Covariances
tr05 ~~ r1_r*r05
tr09 ~~ r2_r*r09
tr13 ~~ r2_r*r13
tr17 ~~ r2_r*r17

tr ~~ r
tr ~~ tr05
tr ~~ r05
r ~~ tr05
r ~~ r05'


