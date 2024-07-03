## Model from Entringer et al., modified with different labels
## Labels (for extracting results):
##
## s_(var) = stability
## r1_(v1v2) = initial wave correlations
## r2_(v1v2) = subsequent wave correlations
## c_r(t) = cross-lag, religion regressed on trait
## c_(t)r = cross-lag, trait regressed on religion

model1_main <- '

#metric invariance (equal loadings)
a05 =~ a*agr0501r + b*agr0502 + c*agr0503
c05 =~ d*cns0501 + e*cns0502r + f*cns0503
e05 =~ g*ext0501 + h*ext0502 + i*ext0503r
n05 =~ j*neu0501 + k*neu0502 + l*neu0503r
o05 =~ m*opn0501 + n*opn0502 + o*opn0503

a09 =~ a*agr0901r + b*agr0902 + c*agr0903
c09 =~ d*cns0901 + e*cns0902r + f*cns0903
e09 =~ g*ext0901 + h*ext0902 + i*ext0903r
n09 =~ j*neu0901 + k*neu0902 + l*neu0903r
o09 =~ m*opn0901 + n*opn0902 + o*opn0903

a13 =~ a*agr1301r + b*agr1302 + c*agr1303
c13 =~ d*cns1301 + e*cns1302r + f*cns1303
e13 =~ g*ext1301 + h*ext1302 + i*ext1303r
n13 =~ j*neu1301 + k*neu1302 + l*neu1303r
o13 =~ m*opn1301 + n*opn1302 + o*opn1303

a17 =~ a*agr1701r + b*agr1702 + c*agr1703
c17 =~ d*cns1701 + e*cns1702r + f*cns1703
e17 =~ g*ext1701 + h*ext1702 + i*ext1703r
n17 =~ j*neu1701 + k*neu1702 + l*neu1703r
o17 =~ m*opn1701 + n*opn1702 + o*opn1703

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
a05	=~	ext0503r
a09	=~	ext0903r
a13	=~	ext1303r
a17	=~	ext1703r

o05	=~	neu0503r
o09	=~	neu0903r
o13	=~	neu1303r
o17	=~	neu1703r

c05	=~	neu0503r
c09	=~	neu0903r
c13	=~	neu1303r
c17	=~	neu1703r

o05	=~	agr0501r
o09	=~	agr0901r
o13	=~	agr1301r
o17	=~	agr1701r


#regression model with equality contraints-----------------------

#cross-lagged effects------------------
relig09 ~ c_ra*a05 + c_rc*c05 + c_re*e05 + c_rn*n05 + c_ro*o05 
relig13 ~ c_ra*a09 + c_rc*c09 + c_re*e09 + c_rn*n09 + c_ro*o09 
relig17 ~ c_ra*a13 + c_rc*c13 + c_re*e13 + c_rn*n13 + c_ro*o13 

a09 ~ c_ar*relig05
c09 ~ c_cr*relig05
e09 ~ c_er*relig05
n09 ~ c_nr*relig05
o09 ~ c_or*relig05

a13 ~ c_ar*relig09
c13 ~ c_cr*relig09
e13 ~ c_er*relig09
n13 ~ c_nr*relig09
o13 ~ c_or*relig09

a17 ~ c_ar*relig13
c17 ~ c_cr*relig13
e17 ~ c_er*relig13
n17 ~ c_nr*relig13
o17 ~ c_or*relig13

#correlated change-------------------------------------
relig05 ~~ r1_ra*a05 + r1_rc*c05 + r1_re*e05 + r1_rn*n05 + r1_ro*o05
relig09 ~~ r2_ra*a09 + r2_rc*c09 + r2_re*e09 + r2_rn*n09 + r2_ro*o09
relig13 ~~ r2_ra*a13 + r2_rc*c13 + r2_re*e13 + r2_rn*n13 + r2_ro*o13
relig17 ~~ r2_ra*a17 + r2_rc*c17 + r2_re*e17 + r2_rn*n17 + r2_ro*o17
a05 ~~ c05 + e05 + n05 + o05
c05 ~~ e05 + n05 + o05
e05 ~~ n05 + o05
n05 ~~ o05
a09 ~~ rac*c09 + rae*e09 + ran*n09 + rao*o09
c09 ~~ rce*e09 + rcn*n09 + rco*o09
e09 ~~ ren*n09 + reo*o09
n09 ~~ rno*o09
a13 ~~ rac*c13 + rae*e13 + ran*n13 + rao*o13
c13 ~~ rce*e13 + rcn*n13 + rco*o13
e13 ~~ ren*n13 + reo*o13
n13 ~~ rno*o13
a17 ~~ rac*c17 + rae*e17 + ran*n17 + rao*o17
c17 ~~ rce*e17 + rcn*n17 + rco*o17
e17 ~~ ren*n17 + reo*o17
n17 ~~ rno*o17




#autocorrelations------------------
relig09 ~ sr*relig05
relig13 ~ sr*relig09
relig17 ~ sr*relig13

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
'

