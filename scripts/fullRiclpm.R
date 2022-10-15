## RI-CLPM model with latent traits
## Modified from Entringer et al. original model
##
## Labels (for extracting results):
##
## s_(var) = stability
## r1_(v1v2) = initial wave correlations
## r2_(v1v2) = subsequent wave correlations
## c_r(t) = cross-lag, religion regressed on trait
## c_(t)r = cross-lag, trait regressed on religion
## ri_(v1v2) = correlation between random intercepts

model1_riclpm <- '

#metric invariance (equal loadings)
## Modified from original to create within-centered versions
a05o =~ a*agr0501r + b*agr0502 + c*agr0503
c05o =~ d*cns0501 + e*cns0502r + f*cns0503
e05o =~ g*ext0501 + h*ext0502 + i*ext0503r
n05o =~ j*neu0501 + k*neu0502 + l*neu0503r
o05o =~ m*opn0501 + n*opn0502 + o*opn0503

a09o =~ a*agr0901r + b*agr0902 + c*agr0903
c09o =~ d*cns0901 + e*cns0902r + f*cns0903
e09o =~ g*ext0901 + h*ext0902 + i*ext0903r
n09o =~ j*neu0901 + k*neu0902 + l*neu0903r
o09o =~ m*opn0901 + n*opn0902 + o*opn0903

a13o =~ a*agr1301r + b*agr1302 + c*agr1303
c13o =~ d*cns1301 + e*cns1302r + f*cns1303
e13o =~ g*ext1301 + h*ext1302 + i*ext1303r
n13o =~ j*neu1301 + k*neu1302 + l*neu1303r
o13o =~ m*opn1301 + n*opn1302 + o*opn1303

a17o =~ a*agr1701r + b*agr1702 + c*agr1703
c17o =~ d*cns1701 + e*cns1702r + f*cns1703
e17o =~ g*ext1701 + h*ext1702 + i*ext1703r
n17o =~ j*neu1701 + k*neu1702 + l*neu1703r
o17o =~ m*opn1701 + n*opn1702 + o*opn1703

## Create within-centered variables
a05 =~ 1*a05o
c05 =~ 1*c05o
e05 =~ 1*e05o
n05 =~ 1*n05o
o05 =~ 1*o05o

a09 =~ 1*a09o
c09 =~ 1*c09o
e09 =~ 1*e09o
n09 =~ 1*n09o
o09 =~ 1*o09o

a13 =~ 1*a13o
c13 =~ 1*c13o
e13 =~ 1*e13o
n13 =~ 1*n13o
o13 =~ 1*o13o

a17 =~ 1*a17o
c17 =~ 1*c17o
e17 =~ 1*e17o
n17 =~ 1*n17o
o17 =~ 1*o17o

relig05w =~ 1*relig05
relig09w =~ 1*relig09
relig13w =~ 1*relig13
relig17w =~ 1*relig17

## Create Random Intercepts

ri_a =~ 1*a05o + 1*a09o + 1*a13o + 1*a17o
ri_c =~ 1*c05o + 1*c09o + 1*c13o + 1*c17o
ri_e =~ 1*e05o + 1*e09o + 1*e13o + 1*e17o
ri_n =~ 1*n05o + 1*n09o + 1*n13o + 1*n17o
ri_o =~ 1*o05o + 1*o09o + 1*o13o + 1*o17o
ri_r =~ 1*relig05 + 1*relig09 + 1*relig13 + 1*relig17

#correlated residuals -- correlated items
agr0501r ~~ agr0901r
agr0501r ~~ agr1301r
agr0501r ~~ agr1701r
agr0901r ~~ agr1301r
agr0901r ~~ agr1701r
agr1701r ~~ agr1301r

agr0502 ~~ agr0902
agr0502 ~~ agr1302
agr0502 ~~ agr1702
agr0902 ~~ agr1302
agr0902 ~~ agr1702
agr1302 ~~ agr1702

agr0503 ~~ agr0903
agr0503 ~~ agr1303
agr0503 ~~ agr1703
agr0903 ~~ agr1303
agr0903 ~~ agr1703
agr1303 ~~ agr1703


cns0501 ~~ cns0901
cns0501 ~~ cns1301
cns0501 ~~ cns1701
cns1301 ~~ cns0901
cns1701 ~~ cns0901
cns1701 ~~ cns1301

cns0502r ~~ cns0902r
cns0502r ~~ cns1302r
cns0502r ~~ cns1702r
cns1302r ~~ cns0902r
cns1702r ~~ cns0902r
cns1702r ~~ cns1302r

cns0503 ~~ cns0903
cns0503 ~~ cns1303
cns0503 ~~ cns1703
cns1303 ~~ cns0903
cns1703 ~~ cns0903
cns1703 ~~ cns1303


ext0501 ~~ ext0901
ext0501 ~~ ext1301
ext0501 ~~ ext1701
ext1301 ~~ ext0901
ext1701 ~~ ext0901
ext1701 ~~ ext1301

ext0502 ~~ ext0902
ext0502 ~~ ext1302
ext0502 ~~ ext1702
ext1302 ~~ ext0902
ext1302 ~~ ext1702

ext0503r ~~ ext0903r
ext0503r ~~ ext1303r
ext0503r ~~ ext1703r
ext1303r ~~ ext0903r
ext1703r ~~ ext0903r
ext1703r ~~ ext1303r

neu0501 ~~ neu0901
neu0501 ~~ neu1301
neu0501 ~~ neu1701
neu1301 ~~ neu0901
neu1701 ~~ neu0901
neu1701 ~~ neu1301

neu0502 ~~ neu0902
neu0502 ~~ neu1302
neu0502 ~~ neu1702
neu1302 ~~ neu0902
neu1702 ~~ neu0902
neu1302 ~~ neu1702

neu0503r ~~ neu0903r
neu0503r ~~ neu1303r
neu0503r ~~ neu1703r
neu1303r ~~ neu0903r
neu1703r ~~ neu0903r
neu1703r ~~ neu1303r

opn0501 ~~ opn0901
opn0501 ~~ opn1301
opn0501 ~~ opn1701
opn1301 ~~ opn0901
opn1701 ~~ opn0901
opn1301 ~~ opn1701

opn0502 ~~ opn0902
opn0502 ~~ opn1302
opn0502 ~~ opn1702
opn1302 ~~ opn0902
opn1702 ~~ opn0902
opn1702 ~~ opn1302

opn0503 ~~ opn0903
opn0503 ~~ opn1303
opn0503 ~~ opn1703
opn1303 ~~ opn0903
opn1703 ~~ opn0903
opn1703 ~~ opn1303

#crossloadings according to modindices
a05o	=~	ext0503r
a09o	=~	ext0903r
a13o	=~	ext1303r
a17o	=~	ext1703r

o05o	=~	neu0503r
o09o	=~	neu0903r
o13o	=~	neu1303r
o17o	=~	neu1703r

c05o	=~	neu0503r
c09o	=~	neu0903r
c13o	=~	neu1303r
c17o	=~	neu1703r

o05o	=~	agr0501r
o09o	=~	agr0901r
o13o	=~	agr1301r
o17o	=~	agr1701r


#regression model with equality contraints-----------------------

#cross-lagged effects------------------
relig09w ~ c_ra*a05 + c_rc*c05 + c_re*e05 + c_rn*n05 + c_ro*o05 
relig13w ~ c_ra*a09 + c_rc*c09 + c_re*e09 + c_rn*n09 + c_ro*o09 
relig17w ~ c_ra*a13 + c_rc*c13 + c_re*e13 + c_rn*n13 + c_ro*o13 

a09 ~ c_ar*relig05w
c09 ~ c_cr*relig05w
e09 ~ c_er*relig05w
n09 ~ c_nr*relig05w
o09 ~ c_or*relig05w

a13 ~ c_ar*relig09w
c13 ~ c_cr*relig09w
e13 ~ c_er*relig09w
n13 ~ c_nr*relig09w
o13 ~ c_or*relig09w

a17 ~ c_ar*relig13w
c17 ~ c_cr*relig13w
e17 ~ c_er*relig13w
n17 ~ c_nr*relig13w
o17 ~ c_or*relig13w

#correlated change-------------------------------------
relig05w ~~ r1_ra*a05 + r1_rc*c05 + r1_re*e05 + r1_rn*n05 + r1_ro*o05
relig09w ~~ r2_ra*a09 + r2_rc*c09 + r2_re*e09 + r2_rn*n09 + r2_ro*o09
relig13w ~~ r2_ra*a13 + r2_rc*c13 + r2_re*e13 + r2_rn*n13 + r2_ro*o13
relig17w ~~ r2_ra*a17 + r2_rc*c17 + r2_re*e17 + r2_rn*n17 + r2_ro*o17


#autocorrelations------------------
relig09w ~ sr*relig05w
relig13w ~ sr*relig09w
relig17w ~ sr*relig13w

a09 ~ sa*a05
a13 ~ sa*a09
a17 ~ sa*a13

c09 ~ sc*c05
c13 ~ sc*c09
c17 ~ sc*c13

e09 ~ se*e05
e13 ~ se*e09
e17 ~ se*e13

n09 ~ sn*n05
n13 ~ sn*n09
n17 ~ sn*n13

o09 ~ so*o05
o13 ~ so*o09
o17 ~ so*o13


## Variances
relig05 ~~ 0*relig05
relig09 ~~ 0*relig09
relig13 ~~ 0*relig13
relig17 ~~ 0*relig17

a05o ~~ 0*a05o
c05o ~~ 0*c05o
e05o ~~ 0*e05o
n05o ~~ 0*n05o
o05o ~~ 0*o05o

a09o ~~ 0*a09o
c09o ~~ 0*c09o
e09o ~~ 0*e09o
n09o ~~ 0*n09o
o09o ~~ 0*o09o

a13o ~~ 0*a13o
c13o ~~ 0*c13o
e13o ~~ 0*e13o
n13o ~~ 0*n13o
o13o ~~ 0*o13o

a17o ~~ 0*a17o
c17o ~~ 0*c17o
e17o ~~ 0*e17o
n17o ~~ 0*n17o
o17o ~~ 0*o17o

## Random Intercept Correlations

ri_a ~~ ri_ac*ri_c
ri_a ~~ ri_ae*ri_e
ri_a ~~ ri_an*ri_n
ri_a ~~ ri_ao*ri_o
ri_a ~~ ri_ar*ri_r

ri_c ~~ ri_ce*ri_e
ri_c ~~ ri_cn*ri_n
ri_c ~~ ri_co*ri_o
ri_c ~~ ri_cr*ri_r

ri_e ~~ ri_en*ri_n
ri_e ~~ ri_eo*ri_o
ri_e ~~ ri_er*ri_r

ri_n ~~ ri_no*ri_o
ri_n ~~ ri_nr*ri_r

ri_o ~~ ri_or*ri_r

## Constrained to 0

ri_a ~~ 0*a05
ri_a ~~ 0*c05
ri_a ~~ 0*e05
ri_a ~~ 0*n05
ri_a ~~ 0*o05
ri_a ~~ 0*relig05w

ri_c ~~ 0*a05
ri_c ~~ 0*c05
ri_c ~~ 0*e05
ri_c ~~ 0*n05
ri_c ~~ 0*o05
ri_c ~~ 0*relig05w

ri_e ~~ 0*a05
ri_e ~~ 0*c05
ri_e ~~ 0*e05
ri_e ~~ 0*n05
ri_e ~~ 0*o05
ri_e ~~ 0*relig05w

ri_n ~~ 0*a05
ri_n ~~ 0*c05
ri_n ~~ 0*e05
ri_n ~~ 0*n05
ri_n ~~ 0*o05
ri_n ~~ 0*relig05w

ri_o ~~ 0*a05
ri_o ~~ 0*c05
ri_o ~~ 0*e05
ri_o ~~ 0*n05
ri_o ~~ 0*o05
ri_o ~~ 0*relig05w

ri_r ~~ 0*a05
ri_r ~~ 0*c05
ri_r ~~ 0*e05
ri_r ~~ 0*n05
ri_r ~~ 0*o05
ri_r ~~ 0*relig05w

'
