library(randomSubspace)
library(e1071)
library(randomForest)

# This will be used if the MODEL environment variable is set to "rforest"
rforest <- function(x, y) randomForest(x, y, ntree=5)

verbose <- as.logical(Sys.getenv('VERBOSE', 'FALSE'))
model <- match.fun(Sys.getenv('MODEL', 'svm'))

if (as.logical(Sys.getenv('MULTICORE', 'FALSE'))) {
  library(doMC)
  registerDoMC()
} else if (as.logical(Sys.getenv('MPI', 'FALSE'))) {
  library(doMPI)
  cl <- startMPIcluster(verbose=verbose)
  registerDoMPI(cl)
} else {
  registerDoSEQ()
}

data(AMLALL_train)
data(AMLALL_test)

training.feature <- AMLALL_train[,-7130]
training.class <- AMLALL_train[,7130]

test.feature <- AMLALL_test[,-7130]
test.class <- AMLALL_test[,7130]

cat('Executing randomSubspace\n')
etime <- system.time({
  rs <- randomSubspace(training.feature, training.class, test.feature, test.class,
                       sub.dim=50, ntree=500, cv=3, w.type=0, verbose=verbose,
                       model=model)
})
cat('Time:\n')
print(etime)

stopifnot(is.null(rs$weight))
stopifnot(rs$w.type == 0)

# Compare different weight assignment procedures
err.misc <- list(0)
for(i in 1:3) {
  cat(sprintf('Executing class.err with w.type %d\n', i))
  etime <- system.time({
    err.misc[[i]] <- class.err(rs, step.size=10, w.type=i, verbose=verbose)
  })
  cat('Time:\n')
  print(etime)
}

print(err.misc)
