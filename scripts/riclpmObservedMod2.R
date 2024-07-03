riclpm_observed <- '

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

## Random Intercepts

ri_agr =~ 1*agr05 + 1*agr09 + 1*agr13 + 1*agr17
ri_cns =~ 1*cns05 + 1*cns09 + 1*cns13 + 1*cns17
ri_ext =~ 1*ext05 + 1*ext09 + 1*ext13 + 1*ext17
ri_neu =~ 1*neu05 + 1*neu09 + 1*neu13 + 1*neu17
ri_opn =~ 1*opn05 + 1*opn09 + 1*opn13 + 1*opn17
ri_relig =~ 1*relig05 + 1*relig09 + 1*relig13 + 1*relig17

## Stabilities

wagr09 ~ sa*wagr05
wagr13 ~ sa*wagr09
wagr17 ~ sa*wagr13

wcns09 ~ sc*wcns05
wcns13 ~ sc*wcns09
wcns17 ~ sc*wcns13

wext09 ~ se*wext05
wext13 ~ se*wext09
wext17 ~ se*wext13

wneu09 ~ sn*wneu05
wneu13 ~ sn*wneu09
wneu17 ~ sn*wneu13

wopn09 ~ so*wopn05
wopn13 ~ so*wopn09
wopn17 ~ so*wopn13

wrelig09 ~ sr*wrelig05
wrelig13 ~ sr*wrelig09
wrelig17 ~ sr*wrelig13

## Cross-lagged paths

wrelig09 ~ c_ra*wagr05 + c_rc*wcns05 + c_re*wext05 + c_rn*wneu05 + c_ro*wopn05
wrelig13 ~ c_ra*wagr09 + c_rc*wcns09 + c_re*wext09 + c_rn*wneu09 + c_ro*wopn09
wrelig17 ~ c_ra*wagr13 + c_rc*wcns13 + c_re*wext13 + c_rn*wneu13 + c_ro*wopn13

wagr09 ~ c_ar*wrelig05
wcns09 ~ c_cr*wrelig05
wext09 ~ c_er*wrelig05
wneu09 ~ c_nr*wrelig05
wopn09 ~ c_or*wrelig05

wagr13 ~ c_ar*wrelig09
wcns13 ~ c_cr*wrelig09
wext13 ~ c_er*wrelig09
wneu13 ~ c_nr*wrelig09
wopn13 ~ c_or*wrelig09

wagr17 ~ c_ar*wrelig13
wcns17 ~ c_cr*wrelig13
wext17 ~ c_er*wrelig13
wneu17 ~ c_nr*wrelig13
wopn17 ~ c_or*wrelig13

## Correlations

wrelig05 ~~ r1_ra*wagr05 + r1_rc*wcns05 + r1_re*wext05 + r1_rn*wneu05 + r1_ro*wopn05
wagr05 ~~ wcns05 + wext05 + wneu05 + wopn05
wcns05 ~~ wext05 + wneu05 + wopn05
wext05 ~~ wneu05 + wopn05
wneu05 ~~ wopn05

wrelig09 ~~ r2_ra*wagr09 + r2_rc*wcns09 + r2_re*wext09 + r2_rn*wneu09 + r2_ro*wopn09
wagr09 ~~ rac*wcns09 + rae*wext09 + ran*wneu09 + rao*wopn09
wcns09 ~~ rce*wext09 + rcn*wneu09 + rco*wopn09
wext09 ~~ ren*wneu09 + reo*wopn09
wneu09 ~~ rno*wopn09

wrelig13 ~~ r2_ra*wagr13 + r2_rc*wcns13 + r2_re*wext13 + r2_rn*wneu13 + r2_ro*wopn13
wagr13 ~~ rac*wcns13 + rae*wext13 + ran*wneu13 + rao*wopn13
wcns13 ~~ rce*wext13 + rcn*wneu13 + rco*wopn13
wext13 ~~ ren*wneu13 + reo*wopn13
wneu13 ~~ rno*wopn13

wrelig17 ~~ r2_ra*wagr17 + r2_rc*wcns17 + r2_re*wext17 + r2_rn*wneu17 + r2_ro*wopn17
wagr17 ~~ rac*wcns17 + rae*wext17 + ran*wneu17 + rao*wopn17
wcns17 ~~ rce*wext17 + rcn*wneu17 + rco*wopn17
wext17 ~~ ren*wneu17 + reo*wopn17
wneu17 ~~ rno*wopn17

## Correlations constrained to zero

ri_relig ~~ 0*wrelig05 + 0*wagr05 + 0*wcns05 + 0*wext05 + 0*wneu05 + 0*wopn05
ri_agr ~~ 0*wrelig05 + 0*wagr05 + 0*wcns05 + 0*wext05 + 0*wneu05 + 0*wopn05
ri_cns ~~ 0*wrelig05 + 0*wagr05 + 0*wcns05 + 0*wext05 + 0*wneu05 + 0*wopn05
ri_ext ~~ 0*wrelig05 + 0*wagr05 + 0*wcns05 + 0*wext05 + 0*wneu05 + 0*wopn05
ri_neu ~~ 0*wrelig05 + 0*wagr05 + 0*wcns05 + 0*wext05 + 0*wneu05 + 0*wopn05
ri_opn ~~ 0*wrelig05 + 0*wagr05 + 0*wcns05 + 0*wext05 + 0*wneu05 + 0*wopn05

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

ri_relig ~~ ri_relig
ri_agr ~~ ri_agr
ri_cns ~~ ri_cns
ri_ext ~~ ri_ext
ri_neu ~~ ri_neu
ri_opn ~~ ri_opn



'
