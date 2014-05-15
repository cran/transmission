library(testthat)
library(transmission)
library(plyr)
context("Input formats")
testthat::test_that("Indiffernce to input formats.", {
    # generate data
    set.seed(20111228)
    simdf <- doSim(cap=100, trans=0.01, imp=0.05,  test=7.5, stay=5.5, len=100, fn=0.05)
    N <- 10L
    
    # Patient/Test format
    mcmc.pt <- new(cont.inf.model)
    mcmc.pt$load(simdf$patients, simdf$tests)
    
    # Event format
    e1 <- arrange(transmission:::.convertForAlun(simdf), time, pid, event)
    seed <- .Random.seed
    mcmc.e1 <- new(cont.inf.model)
    mcmc.e1$load_by_events(e1)
    
    e1 <- mcmc.pt$events
    e2 <- mcmc.e1$events
    expect_that(e1, equals(e2))
    
    # Event Format sorted by pid, time, then event
    e2 <- arrange(transmission:::.convertForAlun(simdf), pid, time, event)
    .Random.seed <<- seed
    mcmc.e2 <- new(cont.inf.model)
    mcmc.e2$load_by_events(e2)
    
    expect_that(mcmc.pt$events, equals(mcmc.e2$events))
    expect_that(mcmc.e1$events, equals(mcmc.e2$events))
    
    expect_that(mcmc.pt$transmission, equals(mcmc.e1$transmission))
    expect_that(mcmc.pt$transmission, equals(mcmc.e2$transmission))
    expect_that(mcmc.e1$transmission, equals(mcmc.e2$transmission))
    
    expect_that(mcmc.pt$importation, equals(mcmc.e1$importation))
    expect_that(mcmc.pt$importation, equals(mcmc.e2$importation))
    expect_that(mcmc.e1$importation, equals(mcmc.e2$importation))

    expect_that(mcmc.pt$false.neg, equals(mcmc.e1$false.neg))
    expect_that(mcmc.pt$false.neg, equals(mcmc.e2$false.neg))
    expect_that(mcmc.e1$false.neg, equals(mcmc.e2$false.neg))
    
    expect_that(mcmc.pt$logLik, equals(mcmc.e1$logLik))
    expect_that(mcmc.pt$logLik, equals(mcmc.e2$logLik))
    expect_that(mcmc.e1$logLik, equals(mcmc.e2$logLik))
    
    .Random.seed <<- seed; run.pt.start <- mcmc.pt$run(2)
    .Random.seed <<- seed; run.e1.start <- mcmc.e1$run(2)
    .Random.seed <<- seed; run.e2.start <- mcmc.e2$run(2)

    expect_that(mcmc.pt$events, equals(mcmc.e1$events))
    expect_that(mcmc.pt$events, equals(mcmc.e2$events))
    expect_that(mcmc.e1$events, equals(mcmc.e2$events))
    expect_that(run.e1.start, equals(run.pt.start))
    expect_that(run.e2.start, equals(run.pt.start))
    expect_that(run.e1.start, equals(run.e2.start))
    
    .Random.seed <<- seed; run.pt <- mcmc.pt$run(2)
    .Random.seed <<- seed; run.e1 <- mcmc.e1$run(2)
    .Random.seed <<- seed; run.e2 <- mcmc.e2$run(2)
    
    expect_that(mcmc.pt$events, equals(mcmc.e1$events))
    expect_that(mcmc.pt$events, equals(mcmc.e2$events))
    expect_that(mcmc.e1$events, equals(mcmc.e2$events))
    
    expect_that(run.pt, is_a("data.frame"))
    expect_that(run.e1, is_a("data.frame"))
    expect_that(run.e2, is_a("data.frame"))
    
    expect_that(run.e1[1, ], equals(run.pt[1, ]))
    expect_that(run.e2[1, ], equals(run.pt[1, ]))
    expect_that(run.e1[1, ], equals(run.e2[1, ]))

    expect_that(run.e1, equals(run.pt))
    expect_that(run.e2, equals(run.pt))
    expect_that(run.e1, equals(run.e2))
})
