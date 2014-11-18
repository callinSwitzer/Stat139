2*(1- pt(1.414, df = 83)) # two tailed p-value


2*(1-pt(2.347, df = 44))


#ln(Xi/Yi) = [0.009026, 0.0483]

exp(0.009026)
exp(0.0483)


# sample binomial sample
p = .43
ss <- sample(c(1,0), size = 100, replace = T, prob = c(p, 1-p))
democ <- sum(ss)
rep <- 100 - democ
democ
rep

pollRep <- function(p = 0.43){
     ss <- sample(c(0,1), size = 100, replace = T, prob = c(p, 1-p))
     democ <- sum(ss)
     rep <- 100 - democ
     c(rep)
}

sd(replicate(53, pollRep()))

foo <- function(reps = 2){
     ff <- replicate(reps,pollRep())
     mean(ff)
}


nnd <- replicate(2, foo(reps = 300))
nnd
var(nnd)
var(ff)

# sample 53 polls with p = .465
ffo <- function(npolls = 53){
     sd(replicate(npolls, pollRep(p=.465)))
}

mean(replicate(1000, ffo(npolls = 53)))
sqrt((.465* (1-.465)) / 52)
#foo
var(foo)

sqrt(53*.465*(1-.465))


var(ss)
mean(ss)

n = 100
p*(1-p)

binrep <- function(o){
     bin <- rbinom(100, size = 1, prob =  0.05)
     #hist(bin)
     bin <- bin*100
     c(var(bin), sd(bin) )
}



foo <- replicate(52, binrep())
foo <- t(foo)
mean(foo[,2])

mean(replicate(10000, sd(rbinom(100, size = 53, prob =  0.523))))



## try binomial
npolls = 53
p = 46.5

sqrt(npolls* (p/100 * (1-p/100)))

sqrt(1502 * (45/100) * (1-45/100))/1502 * 100

sqrt(qf(0.95, 2, 28)*2)

qt(p = 0.975,28 )

1-pnorm(1/sqrt(2))

g1 <- rep(1, 10)
g2 <- rep(0, 10)
??perm.test
library(coin)

n = 10
g1 <- rep(1, n)
g2 <- rep(0, n)
perm.test(g1, g2, paired = T)

1-pnorm(1/sqrt(2))
