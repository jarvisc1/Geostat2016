# Predict Parent class TAXOUSDA for validation data
rm(list=ls())
load('dat/ML.rdata')

validate_p <- validate
# Voronoi # about 45% predictions

library(dismo)
voron <- function(dat, folds = 5){
  kf <- kfold(nrow(dat), k =folds)
  rcm <- list()
  cm <- rep(NA, folds)
  for (k in 1:folds) {
    test <- dat[kf == k, ]
    train <- dat[kf != k, ]
    v <- voronoi(train)
    pp <- raster::extract(v, test)
    rcm[k] <- list(test$TAXOUSDA==pp$TAXOUSDA)
    cm[k] <-  table(rcm[1])[2]/length(test)
  }
  return(cm)
}

mean(voron(p, fold = 3))

library(caret)

# Split training data to get idea of how well models work
learn1 <- learn[learn$TAXNUSDA %in% names(table(learn$TAXNUSDA))[table(learn$TAXNUSDA) >= 5],]
trainIndex <- createDataPartition(learn$TAXNUSDA, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train <- tbp_df[ trainIndex, ]
train$TAXOUSDA <- learn$TAXOUSDA[ trainIndex]
train_cont <- train[, !names(train)%in% c("LNDCOV6_100m", "PMTGSS7_100m", "TAXNUSDA")]

### full case
train_cc <- train[!is.na(train$LNDCOV6_100m), ]
train_cc <- train_cc[!is.na(train_cc$PMTGSS7_100m), ]
test  <- tbp_df[-trainIndex, ]
test$TAXOUSDA <- learn$TAXOUSDA[-trainIndex]


# Set repeated cross validation with 3 repeated and 10 folds
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5,
                     repeats = 3)

# svmLinear model # continuous vars
svmL_mod <- train(TAXOUSDA~.,data=train_cont,method="svmLinear",
                trControl=ctrl,
                allowParallel=TRUE)
predsvmL <- predict(svmL_mod, newdata = test)
length(predsvmL)
cmsvmL <- confusionMatrix(predsvmL, test$TAXOUSDA)
cmsvmL$overall[1] # 0.549

# Predict on validation dataset
predsvmL_v <- predict(svmL_mod, newdata = vbp_df)
length(predsvmL_v)
validate_p$svmL_mod <- predsvmL_v
validate_parent_per <- c(cmsvmL$overall[1])
readr::write_csv(validate_p, "dat/validate_parent.csv")
save(validate_parent_per, file="validate_parent_per.r")

# svmRadial model # continuous vars
svmR_mod <- train(TAXOUSDA~.,data=train_cont,method="svmRadial",
                trControl=ctrl,
                allowParallel=TRUE)

predsvmR <- predict(svmR_mod, newdata = test)
length(predsvmR)
cmsvmR <- confusionMatrix(predsvmR, test$TAXOUSDA)
cmsvmR$overall[1] # 0.58

# Predict on validation dataset
predsvmR_v <- predict(svmR_mod, newdata = vbp_df)
length(predsvmR_v)
validate_p <- readr::read_csv("dat/validate_parent.csv")
validate_p$svmR_mod <- predsvmR_v

readr::write_csv(validate_p, "dat/validate_parent.csv")
load("validate_parent_per.r")
validate_parent_per <- c(validate_parent_per, cmsvmR$overall[1])
save(validate_parent_per, file="validate_parent_per.r")

# RF model # continuous vars
rf_mod <- train(TAXOUSDA~.,data=train_cont,method="rf",
                trControl=ctrl,
                allowParallel=TRUE)

predrf <- predict(rf_mod, newdata = test)
length(predrf)
cmrf <- confusionMatrix(predrf, test$TAXOUSDA)
cmrf$overall[1] # 0.5984

# Predict on validation dataset
predrf_v <- predict(rf_mod, newdata = vbp_df)
length(predrf_v)
validate_p <- readr::read_csv("dat/validate_parent.csv")
validate_p$rf_mod <- predrf_v

readr::write_csv(validate_p, "dat/validate_parent.csv")
load("validate_parent_per.r")
validate_parent_per <- c(validate_parent_per, cmrf$overall[1])
save(validate_parent_per, file="validate_parent_per.r")

# RF model # factor vars
rf1_mod <- train(TAXOUSDA~.,data=train_cc,method="rf",
                trControl=ctrl,
                allowParallel=TRUE)
length(test$TAXOUSDA)
predrf1 <- predict(rf1_mod, newdata = test)
length(predrf1)
length(test$TAXOUSDA[!(is.na(test$LNDCOV6_100m)|is.na(test$PMTGSS7_100m))])
cmrf1 <- confusionMatrix(predrf1, test$TAXOUSDA[!(is.na(test$LNDCOV6_100m)|is.na(test$PMTGSS7_100m))])
cmrf1$overall[1] # 0.6018663

# Predict on validation dataset # different length so slightly different process
predrf1_v <- predict(rf1_mod, newdata = vbp_df)
length(predrf1_v)
vbpIndex <- !(is.na(vbp_df$LNDCOV6_100m)|is.na(vbp_df$PMTGSS7_100m))
vbpcoord <- validate[vbpIndex,]
vbpcoord$predrf1_v <- predrf1_v
validate_p <- readr::read_csv("dat/validate_parent.csv")
validate_p <- dplyr::left_join(validate_p, vbpcoord)

readr::write_csv(validate_p, "dat/validate_parent.csv")
load("validate_parent_per.r")
validate_parent_per <- c(validate_parent_per, cmrf1$overall[1])
save(validate_parent_per, file="validate_parent_per.r")

# RF model # just factor vars
rf2_mod <- train(TAXOUSDA~ LNDCOV6_100m + PMTGSS7_100m,data=train_cc,method="rf",
                trControl=ctrl,
                allowParallel=TRUE)
length(test$TAXOUSDA)
predrf2 <- predict(rf2_mod, newdata = test)
length(predrf2)
length(test$TAXOUSDA[!(is.na(test$LNDCOV6_100m)|is.na(test$PMTGSS7_100m))])
cmrf2 <- confusionMatrix(predrf2, test$TAXOUSDA[!(is.na(test$LNDCOV6_100m)|is.na(test$PMTGSS7_100m))])
cmrf2$overall[1] # 0.4696734

# Predict on validation dataset # different length so slightly different process
predrf2_v <- predict(rf2_mod, newdata = vbp_df)
length(predrf2_v)
vbpIndex <- !(is.na(vbp_df$LNDCOV6_100m)|is.na(vbp_df$PMTGSS7_100m))
vbpcoord <- validate[vbpIndex,]
vbpcoord$predrf2_v <- predrf2_v
validate_p <- readr::read_csv("dat/validate_parent.csv")
validate_p <- dplyr::left_join(validate_p, vbpcoord)

readr::write_csv(validate_p, "dat/validate_parent.csv")
load("validate_parent_per.r")
validate_parent_per <- c(validate_parent_per, cmrf2$overall[1])
save(validate_parent_per, file="validate_parent_per.r")