dpm <- '#!! Autoregressive Part for X
#!! Regression Statements
x2 ~ a*x1
x3 ~ a*x2
x4 ~ a*x3
#!! Autoregressive Component Variance for X
x1 ~~ vx*x1
x2 ~~ vx2*x2
x3 ~~ vx3*x3
x4 ~~ vx4*x4
#!! Stable Trait X
traitX =~ 1*x2
traitX =~ 1*x3
traitX =~ 1*x4
#!! Trait Variance for X
traitX ~~ tx*traitX
#!! Autoregressive Part for Y
#!! Regression Statements
y2 ~ b*y1
y3 ~ b*y2
y4 ~ b*y3
#!! Autoregressive Component Variance for Y
y1 ~~ vy*y1
y2 ~~ vy2*y2
y3 ~~ vy3*y3
y4 ~~ vy4*y4
#!! Stable Trait Y
traitY =~ 1*y2
traitY =~ 1*y3
traitY =~ 1*y4
#!! Trait Variance for Y
traitY ~~ ty*traitY
#!! Cross-Lagged Paths
#!! Y Predicted from X
y2 ~ c*x1
y3 ~ c*x2
y4 ~ c*x3
#!! Cross-Lagged Paths
#!! X Predicted from Y
x2 ~ d*y1
x3 ~ d*y2
x4 ~ d*y3
#!! Stable Trait Correlations
traitX ~~ cov_txty*traitY
#!! AR Correlations
x1 ~~ cov_xy*y1
x2 ~~ y2
x3 ~~ y3
x4 ~~ y4
#!! Correlations between Stable Traits and AR
traitX ~~ x1
traitY ~~ y1
traitX ~~ y1
traitY ~~ x1


tx > 0
ty > 0
vx > 0
vy > 0'
