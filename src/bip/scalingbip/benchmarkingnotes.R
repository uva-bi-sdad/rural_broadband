library(microbenchmark)

microbenchmark(read.csv("movies.csv"),
                 readRDS("movies.rds"),
                 times = 10)

library(benchmarkme)

res <- benchmark_st(runs = 3) #standard operations
upload_results(res)


library(profvis)


profivis({
  #literally paste in an R script/chunk
  
})

# && instead of &
## vectorized solutions

library(parallel)
detectCores()
copiesR <- detectCores() - 1 #number of cores - 1
c1 <- makeCluster(copiesR)
m <- data.frame(rnorm (100), rnorm(100))
parApply(c1, m, 1, median)
stopCluster(c1)
library(benchmarkme)
get_cpu()

## Bootstrapping - sampling repeatedly from same dataset
# with replacement - overlapping sampls
# without replacement - non overlapping samples

copiesR <- detectCores() - 1 #number of cores - 1
c1 <- makeCluster(copiesR)
# m <- data.frame(rnorm (100), rnorm(100))
clusterExport(c1, c("bootstrap", "pokemon"))
# parApply(c1, m, 1, median)
stopCluster(c1)

