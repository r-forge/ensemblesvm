library(randomSubspace)
library(randomForest)

rforest <- function(x, y) randomForest(x, y, ntree=5)

data(AMLALL_train)
data(AMLALL_test)

training.feature <- AMLALL_train[,-7130]
training.class <- AMLALL_train[,7130]

test.feature <- AMLALL_test[,-7130]
test.class <- AMLALL_test[,7130]

rs <- randomSubspace(training.feature, training.class, model=rforest, ntree=100)
print(rs)
print(rs$forest[[1]]$fit)
stopifnot(rs$w.type == 2)

y.rs <- predict(rs, test.feature, w.type=1)
cat(sprintf("randomSubspace error fraction for w.type %d: %f\n",
            1, sum(y.rs != test.class) / length(test.class)))

y.rs <- predict(rs, test.feature)
cat(sprintf("randomSubspace error fraction for w.type %d: %f\n",
            2, sum(y.rs != test.class) / length(test.class)))

y.rs <- predict(rs, test.feature, w.type=3)
cat(sprintf("randomSubspace error fraction for w.type %d: %f\n",
            3, sum(y.rs != test.class) / length(test.class)))

s <- svm(training.feature, training.class)
print(s)
y.s <- predict(s, test.feature)
cat(sprintf("svm error fraction: %f\n", sum(y.s != test.class) / length(test.class)))

rf <- randomForest(training.feature, training.class)
print(rf)
y.rf <- predict(rf, test.feature)
cat(sprintf("randomForest error fraction: %f\n", sum(y.rf != test.class) / length(test.class)))
