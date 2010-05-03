########################################################################
#
# Random Subspace Method
#
########################################################################

# Note: the default value of the sub.dim argument was taken from the
# mtry argument of the randomForest function.
# I have no idea if it is appropriate.
randomSubspace <- function(x, y, x.test=NULL, y.test=NULL,
                           sub.dim=floor(sqrt(ncol(x))),
                           ntree=500, cv=10, weight=NULL,
                           w.type=if (is.null(weight)) 2 else 0,
                           verbose=FALSE, model=svm) {
  # Sanity check the weight argument
  if (!is.null(weight) && length(weight) != ntree) {
    stop('length of weight is not equal to ntree')
  }

  # Make sure "x" is a matrix, or it will be converted to a data frame
  # everytime the modeling function is called
  if (is.data.frame(x))
    x <- data.matrix(x)

  # Silently force sub.dim into the range 1 to ncol(x)
  sub.dim <- max(1, min(ncol(x), sub.dim))

  # Compute the folds
  folds <- cv.folds(y, cv)
  if (verbose) {
    cat('Cross-validation folds:\n')
    print(folds)
  }

  # Compute the "forest"
  chunkSize <- ceiling(ntree / getDoParWorkers())
  mpiopts <- list(profile=verbose, chunkSize=chunkSize, info=verbose)
  forest <- foreach(icount(ntree), .options.mpi=mpiopts,
                    .packages='randomSubspace') %dopar% {
    subspace <- sample(ncol(x), sub.dim)
    fit <- model(x[,subspace], y)
    mvote <- as.numeric(randomSubspace:::pred.cv(model, x[,subspace],
                                                 y, folds) != y)
    ivotes.test <- if (!is.null(x.test))
      cbind(predict(fit, x.test[,subspace]), seq(length=nrow(x.test)))
    else
      NULL
    list(subspace=subspace, fit=fit, mvote=mvote,
         merr=mean(mvote), ivotes.test=ivotes.test)
  }

  # Compute simil.stat.  But it's only needed if w.type is 2.
  vfun <- function(tree) tree$mvote
  vote <- do.call('cbind', lapply(forest, vfun))
  simil.stat <- crossprod(vote)

  # Compute err.  Used in the print method and if w.type is 2 or 3.
  efun <- function(tree) if (tree$merr == 0) 0.005 else tree$merr
  err <- unlist(lapply(forest, efun))

  # Create the "fit" object
  object <- list(forest=forest, simil.stat=simil.stat, err=err,
                 levels.tr=levels(y), n.tr=nrow(x), w.type=w.type,
                 x.test=x.test, y.test=y.test)
  class(object) <- 'randomSubspace'
  object$weight <- if (w.type <= 0) weight else get.weight(object)
  object
}

# Compute the weights of the trees in a "randomSubspace" object.
# A subset of the forest can be specified using the "nt" argument.
get.weight <- function(object, nt=length(object$forest),
                       w.type=object$w.type) {
  # Silently force nt into the range 1 to length(object$forest)
  nt <- max(1, min(length(object$forest), nt))

  if (w.type == 1) {
    # Equal weight
    rep(1.0 / nt, nt)
  } else if (w.type == 2) {
    # Consider correlation
    Dmat0 <- (2.0 / object$n.tr) * object$simil.stat[1:nt, 1:nt]
    diag(Dmat0) <- 2.0 * object$err[1:nt] + 0.0001
    Amat0 <- cbind(rep(1, nt), diag(rep(1, nt)))
    b0 <- c(1, rep(0, nt))
    solve.QP(Dmat=Dmat0, dvec=rep(0, nt), Amat=Amat0, bvec=b0,
             meq=1)$solution
  } else if (w.type == 3) {
    # Assume independence among weak classifers
    tmp <- log((1 - object$err[1:nt]) / object$err[1:nt])
    tmp / sum(tmp)
  } else {
    stop(sprintf('illegal value of w.type: %s', w.type))
  }
}

# Predict method for "randomSubspace" objects
predict.randomSubspace <- function(object, newdata, weight, w.type, ...) {
  ntree <- length(object$forest)

  if (!missing(weight) && !is.null(weight) && !missing(w.type) &&
      w.type > 0) {
    stop('weight and w.type are set inconsistently')
  }

  weight <- if (!missing(w.type) && w.type > 0) {
    get.weight(object, w.type=w.type)
  } else if (!missing(weight) && !is.null(weight)) {
    if (length(weight) != ntree) {
      stop(sprintf('length of weight is not equal to ntree (%d)', ntree))
    }
    weight
  } else {
    object$weight
  }

  if (is.null(weight)) {
    stop('must specify a valid weight or w.type with predict method')
  }

  obs <- seq(length=nrow(newdata))
  votes <- matrix(0, nrow=length(object$levels.tr), ncol=nrow(newdata))

  # Get the votes from each of the trees
  for (i in seq(along=object$forest)) {
    y <- predict(object$forest[[i]]$fit,
                 newdata[,object$forest[[i]]$subspace], ...)
    ivotes <- cbind(y, obs)
    votes[ivotes] <- votes[ivotes] + weight[i]
  }

  # Determine the winning classifications, returning them as a factor
  winners <- unlist(lapply(obs, function(j) random.which.max(votes[,j])))
  factor(object$levels.tr[winners], levels=object$levels.tr)
}

# Compute the classification error for different numbers of trees
class.err <- function(object, step.size, w.type, verbose=FALSE) {
  # Check if randomSubspace was called with x.test and y.test
  if (is.null(object$x.test) || is.null(object$y.test))
    stop('x.test or y.test was not specified to randomSubspace')

  # Get the total number of trees in this "fit" object
  ntree <- length(object$forest)

  # Vectorizing over the observations
  obs <- seq(length=nrow(object$x.test))

  # Compute the classification error for different numbers of trees
  # Probably only really worthwhile parallelizing this when w.type == 2
  nt <- seq(2, ntree, by=step.size)  # this gets modified in foreach loop
  chunkSize <- ceiling(length(nt) / getDoParWorkers())
  mpiopts <- list(profile=verbose, chunkSize=chunkSize, info=verbose)
  foreach(nt=nt, .combine='c', .options.mpi=mpiopts,
          .packages='randomSubspace') %dopar% {
    weight <- randomSubspace:::get.weight(object, nt, w.type)

    # Get the votes from each of the trees
    votes <- matrix(0, nrow=length(object$levels.tr),
                    ncol=nrow(object$x.test))
    for (i in seq(length=nt)) {
      votes[object$forest[[i]]$ivotes.test] <-
          votes[object$forest[[i]]$ivotes.test] + weight[i]
    }

    # Determine the winning classifications, and compute the
    # classification error
    winners <- unlist(lapply(obs, function(j)
                      randomSubspace:::random.which.max(votes[,j])))
    sum(winners != as.numeric(object$y.test)) / length(object$y.test)
  }
}

# Get the index of one of the maximum elements of x.
# If there are multiple maximum elements, then choose the
# winner randomly, using "sample".  If this is not important,
# then the standard "which.max" function should be used instead,
# since that will be a lot faster.
random.which.max <- function(x) {
  i <- which(x == max(x))
  if (length(i) == 1)
    i
  else if (length(i) > 0)
    sample(i, 1)
  else
    stop('argument to random.which.max has zero length')
}

# Print method for randomSubspace objects
print.randomSubspace <- function(x, ...) {
  ntree <- length(x$forest)
  cat(sprintf('randomSubspace object with %d trees\n', ntree))

  # This is svm-specific
  if (FALSE) {
    cat('Number of support vectors summary:\n')
    sv <- unlist(lapply(x$forest, function(tree) tree$fit$tot.nSV))
    print(summary(sv))
  }

  cat('Error summary:\n')
  print(summary(x$err))

  if (x$w.type < 1 || x$w.type > 3) {
    cat('No weights\n')
  } else {
    cat(sprintf('Weight type: %d\n', x$w.type))
    cat('Weight summary:\n')
    print(summary(x$weight))
  }
  invisible(x)
}

# Perform cross validation using SVM on a data set
pred.cv <- function(model, x, y, folds) {
  y.pr <- y

  for (s in folds) {
    mod <- model(x[-s,,drop=FALSE], y[-s])
    y.pr[s] <- predict(mod, x[s,,drop=FALSE])
  }

  y.pr
}

# Compute a list of folds for cross-validation
cv.folds <- function(y, cv) {
  p <- ((order(as.integer(y)) - 1) %% cv) + 1
  lapply(1:cv, function(i) which(i == p))
}
