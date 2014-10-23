#install.packages("mosaic") # note the quotation m
require(mosaic)
#install.packages("Sleuth2") #
require(Sleuth2)

trellis.par.set(theme = col.mosaic()) # get a better color scheme for lattice
options(digits = 3)

summary(case0101)
favstats(Score ~ Treatment, data = case0101)

histogram(~Score | Treatment, data = case0101)

diffmeans = diff(mean(Score ~ Treatment, data = case0101))
diffmeans 

numsim = 1000 # set to a sufficient number
nulldist = do(numsim) * diff(mean(Score ~ shuffle(Treatment), data = case0101))
confint(nulldist)

histogram(~Intrinsic, nint = 50, data = nulldist, v = c(-4.14, 4.14))


sum(nulldist >= 4.14) + sum(nulldist <= -4.14)
