library(testthat)
library(transmission)
library(plyr)

context("Loading Data")
test_that("Reading in data", {
set.seed(12345)
simdf <- doSim(cap=10, trans=0.01, imp=0.05,  pd=1/4, stay=5.5, len=100, fn=0.05)
mcmc <- new(cont.inf.model)
mcmc$load(simdf$patients, simdf$tests)

# the -1 is for the patient id -1 associated with the beginning marker.
expect_equal(mcmc$nPatients, nrow(simdf$patients))
expect_equal(mcmc$nEvents, 1 + nrow(simdf$patients)*3 + nrow(simdf$tests) )


events <- mcmc$events
expect_equal(mcmc$nEvents, nrow(events))

#check sorted by time
expect_that(events$time, equals(sort(events$time)))

#check for all patients having at admission, discharge and infection.
adt <- subset(events, events$event %in% c(0, 3, 4))
nepp <- daply(adt, .(pid), nrow)
expect_true(all(nepp==3))
})
