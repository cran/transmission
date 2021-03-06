# reproducible.R
library(testthat)
library(transmission)
context("Reproducibility")
test_that("Reproducibility", {
set.seed(20111228)
simdf <- doSim(cap=100, tran=0.01, imp=0.05,  test_rate=7.5, stay=5.5, len=100, fn=0.05)

N <- 1e1

seed <- .Random.seed
mcmc <- new(cont.inf.model)
mcmc$load(simdf$patients, simdf$tests)
run1 <- mcmc$run(N)

.Random.seed <<- seed
mcmc2 <- new(cont.inf.model)
mcmc2$load(simdf$patients, simdf$tests)
run2 <- mcmc2$run(N)

expect_that(run1, equals(run2))
})
