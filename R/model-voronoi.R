library(dismo)

# Fit Voronoi model
set.seed(678)
kf <- kfold(nrow(p))
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

# Print Percentage correct
cm # for each model
mean(cm) # average

# Compare true and false for first model
table(rcm[1])

# Predict for validation and export as csv
v <- voronoi(p)
pp <- raster::extract(v, pv)
readr::write_csv(pp, "dat/predict-voronoi.csv")

