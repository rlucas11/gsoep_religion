clpm_observed <- '

## Create within-person centered variables
## agr, cns, ext, neu, opn

wagr05 =~ 1 * agr05
wagr09 =~ 1 * agr09
wagr13 =~ 1 * agr13
wagr17 =~ 1 * agr17

wcns05 =~ 1 * cns05
wcns09 =~ 1 * cns09
wcns13 =~ 1 * cns13
wcns17 =~ 1 * cns17

wext05 =~ 1 * ext05
wext09 =~ 1 * ext09
wext13 =~ 1 * ext13
wext17 =~ 1 * ext17

wneu05 =~ 1 * neu05
wneu09 =~ 1 * neu09
wneu13 =~ 1 * neu13
wneu17 =~ 1 * neu17

wopn05 =~ 1 * opn05
wopn09 =~ 1 * opn09
wopn13 =~ 1 * opn13
wopn17 =~ 1 * opn17

wrelig05 =~ 1 * relig05
wrelig09 =~ 1 * relig09
wrelig13 =~ 1 * relig13
wrelig17 =~ 1 * relig17

## No Random Intercepts

## Stabilities

wagr09 ~ sta*wagr05
wagr13 ~ sta*wagr09
wagr17 ~ sta*wagr13

wcns09 ~ stc*wcns05
wcns13 ~ stc*wcns09
wcns17 ~ stc*wcns13

wext09 ~ ste*wext05
wext13 ~ ste*wext09
wext17 ~ ste*wext13

wneu09 ~ stn*wneu05
wneu13 ~ stn*wneu09
wneu17 ~ stn*wneu13

wopn09 ~ sto*wopn05
wopn13 ~ sto*wopn09
wopn17 ~ sto*wopn13

wrelig09 ~ str*wrelig05
wrelig13 ~ str*wrelig09
wrelig17 ~ str*wrelig13

## Cross-lagged paths

wrelig09 ~ cla*wagr05 + clc*wcns05 + cle*wext05 + cln*wneu05 + clo*wopn05
wrelig13 ~ cla*wagr09 + clc*wcns09 + cle*wext09 + cln*wneu09 + clo*wopn09
wrelig17 ~ cla*wagr13 + clc*wcns13 + cle*wext13 + cln*wneu13 + clo*wopn13

wagr09 ~ clra*wrelig05
wcns09 ~ clrc*wrelig05
wext09 ~ clre*wrelig05
wneu09 ~ clrn*wrelig05
wopn09 ~ clr0*wrelig05

wagr13 ~ clra*wrelig09
wcns13 ~ clrc*wrelig09
wext13 ~ clre*wrelig09
wneu13 ~ clrn*wrelig09
wopn13 ~ clr0*wrelig09

wagr17 ~ clra*wrelig13
wcns17 ~ clrc*wrelig13
wext17 ~ clre*wrelig13
wneu17 ~ clrn*wrelig13
wopn17 ~ clro*wrelig13

## Correlations

wrelig05 ~~ wagr05 + wcns05 + wext05 + wneu05 + wopn05
wagr05 ~~ wcns05 + wext05 + wneu05 + wopn05
wcns05 ~~ wext05 + wneu05 + wopn05
wext05 ~~ wneu05 + wopn05
wneu05 ~~ wopn05

wrelig09 ~~ r1*wagr09 + r2*wcns09 + r3*wext09 + r4*wneu09 + r5*wopn09
wagr09 ~~ r6*wcns09 + r7*wext09 + r8*wneu09 + r9*wopn09
wcns09 ~~ r10*wext09 + r11*wneu09 + r12*wopn09
wext09 ~~ r13*wneu09 + r14*wopn09
wneu09 ~~ r15*wopn09

wrelig13 ~~ r1*wagr13 + r2*wcns13 + r3*wext13 + r4*wneu13 + r5*wopn13
wagr13 ~~ r6*wcns13 + r7*wext13 + r8*wneu13 + r9*wopn13
wcns13 ~~ r10*wext13 + r11*wneu13 + r12*wopn13
wext13 ~~ r13*wneu13 + r14*wopn13
wneu13 ~~ r15*wopn13

wrelig17 ~~ r1*wagr17 + r2*wcns17 + r3*wext17 + r4*wneu17 + r5*wopn17
wagr17 ~~ r6*wcns17 + r7*wext17 + r8*wneu17 + r9*wopn17
wcns17 ~~ r10*wext17 + r11*wneu17 + r12*wopn17
wext17 ~~ r13*wneu17 + r14*wopn17
wneu17 ~~ r15*wopn17

## Variances constrained to zero

relig05 ~~ 0*relig05
agr05 ~~ 0*agr05
cns05 ~~ 0*cns05
ext05 ~~ 0*ext05
neu05 ~~ 0*neu05
opn05 ~~ 0*opn05

relig09 ~~ 0*relig09
agr09 ~~ 0*agr09
cns09 ~~ 0*cns09
ext09 ~~ 0*ext09
neu09 ~~ 0*neu09
opn09 ~~ 0*opn09

relig13 ~~ 0*relig13
agr13 ~~ 0*agr13
cns13 ~~ 0*cns13
ext13 ~~ 0*ext13
neu13 ~~ 0*neu13
opn13 ~~ 0*opn13

relig17 ~~ 0*relig17
agr17 ~~ 0*agr17
cns17 ~~ 0*cns17
ext17 ~~ 0*ext17
neu17 ~~ 0*neu17
opn17 ~~ 0*opn17

## Variances




'
