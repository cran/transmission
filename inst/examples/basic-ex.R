
set.seed(20111228)
library(transmission)
simdf <- doSim( capacity     = 100
              , transmission = 0.002
              , importation  = 0.05
              , test_rate    = 10
              , stay         = 5.5
              , length       = 365
              , fn = 0.05)
mcmc <- new(cont.inf.model)
mcmc$load(simdf$patients, simdf$tests)

N <- 1e2
run <- mcmc$run(N)

if(require(ggplot2) && require(plyr)){
    mcmc_chain_plot(run)
}
