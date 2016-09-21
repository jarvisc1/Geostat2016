library(dismo)

set.seed(5132015)
kf <- kfold(nrow(p))
k <- 1
rcm <- list()
cm <- rep(NA, 5)
for (k in 1:5) {
  test <- p[kf == k, ]
  train <- p[kf != k, ]
  v <- voronoi(train)
  pp <- raster::extract(v, test)
  rcm[k] <- list(test$TAXNUSDA==pp$TAXNUSDA)
  cm[k] <-  table(rcm[1])[2]/length(test)
}
rcm[1]
# percentage correct
cm
mean(cm)


# raw tabulations
table(rcm[1])
table(rcm[2])
table(rcm[3])
table(rcm[4])
table(rcm[5])
p 



