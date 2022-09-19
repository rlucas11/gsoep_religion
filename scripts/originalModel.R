

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
relig09 ~ aj*a05 + ak*c05 + al*e05 + am*n05 + an*o05 
relig13 ~ aj*a09 + ak*c09 + al*e09 + am*n09 + an*o09 
relig17 ~ aj*a13 + ak*c13 + al*e13 + am*n13 + an*o13 

a09 ~ ao*relig05
c09 ~ ap*relig05
e09 ~ ar*relig05
n09 ~ as*relig05
o09 ~ at*relig05

a13 ~ ao*relig09
c13 ~ ap*relig09
e13 ~ ar*relig09
n13 ~ as*relig09
o13 ~ at*relig09

a17 ~ ao*relig13
c17 ~ ap*relig13
e17 ~ ar*relig13
n17 ~ as*relig13
o17 ~ at*relig13

#correlated change-------------------------------------
relig05 ~~ a05 + c05 + e05 + n05 + o05
relig09 ~~ ae*a09 + af*c09 + ag*e09 + ah*n09 + ai*o09
relig13 ~~ ae*a13 + af*c13 + ag*e13 + ah*n13 + ai*o13
relig17 ~~ ae*a17 + af*c17 + ag*e17 + ah*n17 + ai*o17

#autocorrelations------------------
relig09 ~ au*relig05
relig13 ~ au*relig09
relig17 ~ au*relig13

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
o17 ~ az*o13'

