library(testthat)
library(transmission)
context("simulation")
test_that("sim generates as expected",{
  expect_is(   doSim(len=30, cap=30, trans=0.005, imp=0.1, test=3, stay=5.5), "list")
  expect_error(doSim(len=30, cap=30, trans=0.005, imp=0.1, test=3, stay=  0))
  expect_error(doSim(len=30, cap=30, trans=0.005, imp=0.1, test=3, stay=  0, bal=0))
  expect_error(doSim(len=30, cap=30, trans=0.005, imp=0.1, test=3, stay=  0, bal=10))
  does_not_exceed_capacity <- function(x){
      expectation(sum(is.na(x$getData()$patients$discharge))<=x$capacity
                 , "Current exceeds capacity")
    }
  expect_that( newSimulator(cap=30, trans=0.005, imp=.1, pd_test=1/3
             , stay=5, length=365, balance=1)
             , does_not_exceed_capacity)
})
