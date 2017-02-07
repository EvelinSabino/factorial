library(dplyr)
library(purrr)
library(tidyr)
library(microbenchmark)

# Utility function to check (safely) for n ={0,1,2,...}
# N.B. Functions here convert ultimately to real, not integers, to avoid overflow 
# issues for integer types.
is_natural <- function(n){
  (!grepl("[^[:digit:]]", format(n,  digits = 20, scientific = FALSE)))&&(n>=0)
}

# Base version using a for loop
Factorial_loop <- function(n){
  if (!is_natural(n)) {
    stop("Need a Natural Number")
  }
  
  if (n == 0) {
    return (1)
  } else {
    fact <- 1
    for (x in 1:n) {
      fact <- fact*x
    }
    return (fact)
  }
}

# Using reduce functionality from purrr
Factorial_reduce <- function(n){
  if (!is_natural(n)) {
    stop("Need a Natural Number")
  } 
  
  if (n == 0) {
    return (1)
  } else {
    fact <- reduce(as.double(1:n), function(x,y){x*y})
  }
  return (fact)
}

# Using recursion
Factorial_func <- function(n){
  if (!is_natural(n)) {
    stop("Need a Natural Number")
  } 
  
  if (n == 0) {
    return (1)
  } else {
    fact <- n * Factorial_func(n-1)
  }
  return (fact)
}

# Using memoisation
reset_cache <- function(){
  # Instantiate cache in global environment, encapsulated within function
  assign("cache", c(1,1), envir = .GlobalEnv)  
}

Factorial_mem <- function(n) {
  if (!is_natural(n)) {
    stop("Need a Natural Number")
  }
  
  tryCatch({
    fact <- cache[n+1]
    if (is.na(fact)){
      fact <- n * Factorial_mem(n-1)
      cache <<- c(cache,fact)
    } 
    return (fact)
  },
  error = function(e){
    message("Establishing cache...")
    reset_cache()
    Factorial_mem(n)
  }
  )
}

#--------------------------------------------------------
# Simple utility to run a series of factorials up to Nmax
factory <- function(func, Nmax) {
  unlist(lapply(1:Nmax, FUN = func))
}

# Check all four functions produce the same results
r1 <- factory(Factorial_loop, 20)
r2 <- factory(Factorial_func, 20)
r3 <- factory(Factorial_mem, 20)
r4 <- factory(Factorial_reduce, 20)

simple_test <- all(r1==r2)&&all(r1==r3)&&all(r1==r4)

if (!simple_test) {
  stop("Simple unit test failed - results should all match")
}

#--------------------------------------------------------
# Performance Tests...

message("Running single benchmark, to compare how long it takes to calc n! for n = 50")

# Single shot benchmark
N <-50
reset_cache()
Factorial_mem(N)

# One shot test
single_shot_warm <- microbenchmark(
  Factorial_loop(N),
  Factorial_reduce(N),
  Factorial_func(N),
  Factorial_mem(N)
)

message("...without warm up of cache, (i.e. cache cleared on each run)")

# Single shot benchmark
N <-50
reset_cache()

# One shot test
single_shot_cold <- microbenchmark(
  Factorial_loop(N),
  Factorial_reduce(N),
  Factorial_func(N),
  reset_cache(),
  Factorial_mem(N)
)

message("Running average benchmark, to investigate average of each calculation of n! for the series n = {1,2,3...}")
message("...with reset of cache for each n.")
N <- 100
skip <- 20

tbl_of_means <- data.frame()
for (n in 0:N) {
  reset_cache()
  b_r <- microbenchmark(
    Factorial_loop(n),
    Factorial_reduce(n),
    Factorial_func(n),
    reset_cache(),
    Factorial_mem(n)
  )
  
  means <- b_r %>%
    group_by(expr) %>%
    summarise(mean_time = mean(time)/1000) %>%
    mutate(n = n)
  
  tbl_of_means <- rbind(tbl_of_means, means) 
}

sum_tbl_cold <- tbl_of_means%>%
  filter(n %in% seq(0, N, skip)) %>%
  group_by(expr, n) %>%
  spread(n, mean_time) %>%
  as.data.frame()

message("Running average benchmark, to investigate average of each calculation of n! for the series n = {1,2,3...}")
message("...leaving cache from previous run in tact")

tbl_of_means <- data.frame()
for (n in 0:N) {
  b_r <- microbenchmark(
    Factorial_loop(n),
    Factorial_reduce(n),
    Factorial_func(n),
    Factorial_mem(n)
  )
  
  means <- b_r %>%
    group_by(expr) %>%
    summarise(mean_time = mean(time)/1000) %>%
    mutate(n = n)
  
  tbl_of_means <- rbind(tbl_of_means, means) 
}

sum_tbl_warm <- tbl_of_means%>%
  filter(n %in% seq(0, N, skip)) %>%
  group_by(expr, n) %>%
  spread(n, mean_time) %>%
  as.data.frame()

message("Running cumulative benchmark, to compare how long it takes to calc n! for the series n = {1,2,3...}")
message("...with warm up of cache.")

# Cumulative test, for series
benchmarking_results <- microbenchmark(
  factory(Factorial_loop, N),
  factory(Factorial_reduce, N),
  factory(Factorial_func, N),
  factory(Factorial_mem, N)
)

#--------------------------------------------------------
# write to external file : need to run interactively for sink() to work...
# could rewrite as markdown document...
file_name <- paste(getwd(), "factorial_output.txt", sep="/")
sink(file_name)

cat("Summary performance for one time calculation of 50!: ", sep="\n")
cat("- memoised version looks to be fastest in mean... ", sep="\n")
single_shot_warm
cat(rep("- - -",2), sep="\n")

cat("Summary performance for one time _clean_ calculation of 50!: ", sep="\n")
cat("- cache is cleared each iteration to get performance of cold start calculation...  ",sep="\n")
cat("- memoised version looks uncomfortably slow now, reflecting time to hydrate cache.", sep="\n")
single_shot_cold
cat(rep("- - -",2), sep="\n")

cat("Look at performance of one time clean calculation of N! over a range: ", sep="\n")
cat("- cache is cleared each iteration, to get performance of cold start calculation...  ", sep="\n")
cat("- memoised version looks uncomfortably slow now, loop is fastest", sep="\n")
cat("Unit: microseconds", sep="\n")
sum_tbl_cold
cat(rep("- - -",2), sep="\n")

cat("Look at performance of one time clean calculation of N! over a range: ", sep="\n")
cat("- cache is not cleared after last calculation.", sep="\n")
cat("- memoised version performs better now for large N (~40+)", sep="\n")
cat("Unit: microseconds", sep="\n")
sum_tbl_warm
cat(rep("- - -",2), sep="\n")

cat("Look at average performance of series of incremental calculations of N! over a range: ", sep="\n")
cat("- Cache is not cleared after last calculation.", sep="\n")
cat("- memoised version continues to perform.", sep="\n")

benchmarking_results
cat(rep("- - -",2), sep="\n")

sink()


