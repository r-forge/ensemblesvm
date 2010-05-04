#!/bin/sh

LOGFILE=test.log

exec R --vanilla --slave > ${LOGFILE} 2>&1 <<'EOF'
suppressMessages(library(randomSubspace))
suppressMessages(library(RUnit))

verbose <- as.logical(Sys.getenv('RS_VERBOSE', 'FALSE'))
method <- Sys.getenv('RS_BACKEND', 'SEQ')

if (method == 'SNOW') {
  cat('** Using SNOW backend\n')
  library(doSNOW)
  cl <- makeSOCKcluster(3)
  .Last <- function() {
    cat('shutting down SOCK cluster...\n')
    stopCluster(cl)
    cat('shutdown complete\n')
  }
  registerDoSNOW(cl)
} else if (method == 'MPI') {
  cat('** Using MPI backend\n')
  library(doMPI)
  cl <- if (as.logical(Sys.getenv('USENWS', 'FALSE')))
    startNWScluster(count=2, verbose=verbose)
  else
    startMPIcluster(verbose=verbose, bcast=TRUE)
  .Last <- function() {
    cat('shutting down MPI cluster...\n')
    closeCluster(cl)
    cat('shutdown complete\n')
    if (!as.logical(Sys.getenv('USENWS', 'FALSE')))
      mpi.quit()
  }
  registerDoMPI(cl)
} else if (method == 'MC') {
  cat('** Using multicore backend\n')
  library(doMC)
  registerDoMC()
} else if (method == 'SEQ') {
  cat('** Using sequential backend\n')
  registerDoSEQ()
} else {
  stop('illegal backend specified: ', method)
}

options(warn=1)
options(showWarnCalls=TRUE)

cat('Starting test at', date(), '\n')
cat(sprintf('doPar backend name: %s, version: %s\n', getDoParName(), getDoParVersion()))
cat(sprintf('Running with %d worker(s)\n', getDoParWorkers()))

tests <- c('foldsTest.R')

errcase <- list()
for (f in tests) {
  cat('\nRunning test file:', f, '\n')
  t <- runTestFile(f)
  e <- getErrors(t)
  if (e$nErr + e$nFail > 0) {
    errcase <- c(errcase, t)
    print(t)
  }
}

if (length(errcase) == 0) {
  cat('*** Ran all tests successfully ***\n')
} else {
  cat('!!! Encountered', length(errcase), 'problems !!!\n')
  for (t in errcase) {
    print(t)
  }
}

cat('Finished test at', date(), '\n')
EOF
