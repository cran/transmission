library(testthat)
library(transmission)
library(transmission)

context("Likelihoods")
test_that("Likelihood Computations", {
simdf <- doSim(100, 0.01, 0.05,  7.5, 5.5, 100, fn=0.05)

(ll1 <- GetFullDataLikelihood(tr = 0.01, imp = 0.05, fn = 0.25, 
  patients=simdf$patients, tests=simdf$tests))
m1 <- new(cont.inf.model)
m1$load(simdf$patients, simdf$tests)
m1$transmission = 0.01
m1$importation  = 0.05
m1$fn           = 0.25
m1$fp           = 0
cll <- unlist(m1$logLik)
expect_equivalent(ll1[1], cll)
})
