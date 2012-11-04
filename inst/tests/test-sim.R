library(testthat)
library(transmission)
context("simulation")
test_that("sim generates as expected",{
  expect_is(   doSim(30,.005, .1, 3, 5.5, 30), "list")
  expect_error(doSim(30,0.005, .1, 3, 0, 30))
  expect_error(doSim(30,0.005, .1, 3, 0, 30, 0))
  expect_error(doSim(30,0.005, .1, 3, 0, 30, 10))
  does_not_exceed_capacity <- function(x){
      expectation(sum(is.na(x$getData()$patients$discharge))<=x$capacity
                 , "Current exceeds capacity")
    }
  expect_that( newSimulator(cap=30, trans=0.005, imp=.1, test=3, stay=5, length=365, balance=1)
             , does_not_exceed_capacity)
  #testthat::expect_error(transmission::doSim(0,0.005, .1, 3, 5.5, 30))
  #try(gc(),silent=T) # this will create a warning due to the fail initialization
})
