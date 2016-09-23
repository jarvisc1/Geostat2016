# Aim: Create Submitted MOdel


rm(list=ls())
load('dat/ML.rdata')

# Remove outcome which occur less than 10 times
tbp_df$TAXNUSDA <- as.character(learn$TAXNUSDA)
tbp_df$X <- learn$X
tbp_df$Y <- learn$Y
tbp_df <- tbp_df[tbp_df$TAXNUSDA %in% names(table(tbp_df$TAXNUSDA))[table(tbp_df$TAXNUSDA) >= 5],]
tbp_pca$TAXNUSDA <- as.character(learn$TAXNUSDA)

tbp_pca <- tbp_pca[tbp_pca$TAXNUSDA %in% names(table(tbp_pca$TAXNUSDA))[table(tbp_pca$TAXNUSDA) >= 5],]


# Create dataframe for model


pca <- data.frame(TAXNUSDA = as.character(tbp_df$TAXNUSDA), Comp.1 = tbp_pca$Comp.1,
                    Comp.2 = tbp_pca$Comp.2,
                    Comp.3 = tbp_pca$Comp.3,
                    Comp.4 = tbp_pca$Comp.4,
                    x = tbp_df$X,
                    y = tbp_df$Y,
                    slope = tbp_df$DEMNED6_100m_slope,
                    vdepth = tbp_df$DEMNED6_100m_vdepth,
                    ex2mod = tbp_df$EX2MOD5_100m,
                    pri = tbp_df$PR2PRI5_100m,
                    lnd_c = tbp_df$LNDCOV6_100m,
                    openp =tbp_df$DEMNED6_100m_openp,
                    devmean = tbp_df$DEMNED6_100m_devmean,
                    stringsAsFactors = F)


# Random Forest 
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10,
                     repeats = 3)
mod <- train(TAXNUSDA~., data = pca, method = "rf", trControl = ctrl)

print(mod)
# Create Prediction dataframe

vpca <- data.frame(Comp.1 = vbp_pca$Comp.1,
                    Comp.2 = vbp_pca$Comp.2,
                    Comp.3 = vbp_pca$Comp.3,
                    Comp.4 = vbp_pca$Comp.4,
                    x = validate$X,
                    y = validate$Y,
                    slope = vbp_df$DEMNED6_100m_slope,
                    vdepth = vbp_df$DEMNED6_100m_vdepth,
                    ex2mod = vbp_df$EX2MOD5_100m,
                    pri = vbp_df$PR2PRI5_100m,
                    lnd_c = vbp_df$LNDCOV6_100m,
                    openp =vbp_df$DEMNED6_100m_openp,
                    devmean = vbp_df$DEMNED6_100m_devmean,
                    stringsAsFactors = F)

# Predict outcome
predrf <- predict(mod, newdata = vpca)

# Export as CSV
validate$pred <- predrf
write.csv(validate, 'dat/prediction.csv')
