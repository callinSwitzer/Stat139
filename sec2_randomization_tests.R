##########################
##  STAT 139 Section 2  ##
##  16 Sept 2014        ##
##########################

#########################
## RANDOMIZATION TEST  ##
#########################

#Fisher's sharp null is that there is no treatment effect, ie for all units y(1) = y(0)
y1.null <- y0.null <- c(55,72,72.7,70,66,78.9)

w.obs <- c(1,1,1,0,0,0)     # this is the vector of assignments for the particular
# randomization observed in our experiment

# Under the null, the test statistic is a function only of the 
# treatment assignment vector
test.statistic <- function(w){
  value <- mean(y1.null[w == 1]) - mean(y0.null[w == 0])
  return(value)
}

test.statistic(w.obs)               # what we observed
test.statistic(c(1,0,1,1,0,0))      # value of test statistic for some other randomization

# To get the distribution of the test statistic under the null, we need to compute
# its value for every possible treatment assignment vector
# We'll start with a matrix where each row represents a possible assignment vector
possible.w <- matrix(0,nrow=choose(6,3),ncol=6) 

# How to fill in the 1s?
# This might be a useful function:
?combn
my.combos <- combn(6, 3)
for(i in 1:choose(6,3)){
  possible.w[i, my.combos[, i]] <- 1  # think about this step!!
}

possible.w

# calculate the test statistics under the null
test.stat.null <- apply(possible.w,1,test.statistic)

# Graphically display results
hist(test.stat.null, xlab=expression(bar(Y)[1] - bar(Y)[0]) , breaks=6 )
# Add a vertical line denoting the value of our observed test statistic
abline(v = test.statistic(w.obs), col="red", lwd=2)

# Calculate a (two-sided) p-value
mean(abs(test.statistic(w.obs)) <= abs(test.stat.null) ) # average of (1/0) indicators

# Calculate a (one-sided) p-value
mean(test.stat.null <= test.statistic(w.obs)) # average of (1/0) indicators

# Look at all the possible values that our test statistic can take
table(test.stat.null)

#### What to do if have too many possible treatment assignment mechanisms?
# Make up some fake data
treatment.assignment <- rep(c(0,1), c(50,50))
read.score <- c(rnorm(n=50, mean=69.8, sd=4), rnorm(n=50, mean=70, sd=4))
dat <- data.frame(treatment.assignment, read.score)
# write.csv(dat, file="data/read_score_test.csv")

# Read in dat.csv from STAT139 iSite Section1 folder
# dat <- read.csv("data/read_score_test.csv", header=TRUE)


# Null hypothesis for Fisher's randomization test
y1.null <- y0.null <- dat$read.score
w.obs <- dat$treatment.assignment

# Sample sizes
n0 <- sum(dat$treatment.assignment==0)
n1 <- sum(dat$treatment.assignment==1)
N <- n0+n1


# Can we use the same code as above to run randomization test?
test.statistic(w.obs)   # Our observed test statistic
choose(N, n1)

# Instead of enumerating all the possible assignment vectors
# let's code up the reference distribution of the test statistic
# i.e. we randomly sample from the reference distn

nsamp <- 10000 # How many assignment vectors we will sample
test.stat.null <- rep(NA, nsamp)
for(i in 1:nsamp)
{
  w <- rep(0, N)
  w[sample(N,n1,replace=FALSE)] <- 1
  test.stat.null[i] <- test.statistic(w)
}

# Compare observed test statistic to its reference distn
hist(test.stat.null, xlab="Value of test statistic", main="Randomization Distn of Test Statistic")
abline(v=test.statistic(w.obs),col="red",lwd=2)
mean(abs(test.stat.null) >= abs(test.statistic(w.obs))) # (two-sided) p-value
