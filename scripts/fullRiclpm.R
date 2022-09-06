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
relig09w ~ aj*a05 + ak*c05 + al*e05 + am*n05 + an*o05 
relig13w ~ aj*a09 + ak*c09 + al*e09 + am*n09 + an*o09 
relig17w ~ aj*a13 + ak*c13 + al*e13 + am*n13 + an*o13 

a09 ~ ao*relig05w
c09 ~ ap*relig05w
e09 ~ ar*relig05w
n09 ~ as*relig05w
o09 ~ at*relig05w

a13 ~ ao*relig09w
c13 ~ ap*relig09w
e13 ~ ar*relig09w
n13 ~ as*relig09w
o13 ~ at*relig09w

a17 ~ ao*relig13w
c17 ~ ap*relig13w
e17 ~ ar*relig13w
n17 ~ as*relig13w
o17 ~ at*relig13w

#correlated change-------------------------------------
relig05w ~~ a05 + c05 + e05 + n05 + o05
relig09w ~~ ae*a09 + af*c09 + ag*e09 + ah*n09 + ai*o09
relig13w ~~ ae*a13 + af*c13 + ag*e13 + ah*n13 + ai*o13
relig17w ~~ ae*a17 + af*c17 + ag*e17 + ah*n17 + ai*o17

#autocorrelations------------------
relig09w ~ au*relig05w
relig13w ~ au*relig09w
relig17w ~ au*relig13w

a09 ~ av*a05
a13 ~ av*a09
a17 ~ av*a13

c09 ~ aw*c05
c13 ~ aw*c09
c17 ~ aw*c13

e09 ~ ax*e05
e13 ~ ax*e09
e17 ~ ax*e13

n09 ~ ay*n05
n13 ~ ay*n09
n17 ~ ay*n13

o09 ~ az*o05
o13 ~ az*o09
o17 ~ az*o13

## Variances
relig05 ~~ 0*relig05
relig09 ~~ 0*relig09
relig13 ~~ 0*relig13
relig17 ~~ 0*relig17

a05o ~~ 0*a05o
c05o ~~ 0*c05o
e05o ~~ 0*e05o
n05o ~~ 0*n05o
o05o ~~ 0*o05

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

ri_a ~~ ri_c
ri_a ~~ ri_e
ri_a ~~ ri_n
ri_a ~~ ri_o
ri_a ~~ ri_r

ri_c ~~ ri_e
ri_c ~~ ri_n
ri_c ~~ ri_o
ri_c ~~ ri_r

ri_e ~~ ri_n
ri_e ~~ ri_o
ri_e ~~ ri_r

ri_n ~~ ri_o
ri_n ~~ ri_r

ri_o ~~ ri_r

'
