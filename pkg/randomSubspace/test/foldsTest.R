test01 <- function() {
  for (cv in 1:10) {
    for (n in seq(1, 100, by=3)) {
      for (m in 1:10) {
        y <- factor(sample(letters[1:m], n, replace=TRUE), levels=letters)
        folds <- randomSubspace:::cv.folds(y, cv)

        # Verify that we got the right number of folds
        checkEquals(length(folds), cv)

        # Verify that the concatenation of all folds reference all the rows
        x <- sort(do.call('c', folds))
        checkEquals(x, seq(length=n))

        # Verify that the lengths of the vectors is almost equal
        lengths <- unlist(lapply(folds, length))
        r <- range(lengths)
        checkTrue(r[2] - r[1] <= 1)

        # Verify that each fold contains a similar distribution of values
        t <- lapply(folds, function(f) tabulate(f, nbins=nlevels(y)))
        m <- do.call('pmax', t)
        for (d in t) {
          checkTrue(max(m - d) <= 1)
        }
      }
    }
  }
}
