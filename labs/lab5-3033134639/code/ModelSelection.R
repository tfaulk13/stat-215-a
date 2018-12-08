library(tidyverse)
library(caret)
library(foba)

## CV used for both lasso and ridge -------------------------------------------
cv_model <- trainControl(method = 'cv',
                         number = 10,
                         verboseIter = TRUE)

## Splitting ------------------------------------------------------------------

inTrain <- createDataPartition(y = combined$voxel1, p = 0.8, list = FALSE)
training <- fit_feat[inTrain,]
testing <- fit_feat[-inTrain,]


## Voxel 1 --------------------------------------------------------------------

# Getting cor for each
cor_v1 <- data.frame(cor(resp_dat$voxel1, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v1 > 0.2 | cor_v1 < -0.2)

# Select those columns
df_v1 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v1 <- cbind(resp_dat$voxel1, df_v1)

# Rename column
df_v1 <- df_v1 %>%
  rename(voxel1 = 'resp_dat$voxel1')

# Sampling
inTrain <- createDataPartition(y = df_v1$voxel1, p = 0.8, list = FALSE)
training <- df_v1[inTrain,]
testing <- df_v1[-inTrain,]

# Lasso
lassoFit1 <- train(voxel1 ~ .,
                      data = training,
                      method = 'rqlasso',
                      trControl = cv_model)

# Hold for other regression

## Voxel 2 --------------------------------------------------------------------

# Getting cor for each
cor_v2 <- data.frame(cor(resp_dat$voxel2, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v2 > 0.2 | cor_v2 < -0.2)

# Select those columns
df_v2 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v2 <- cbind(resp_dat$voxel2, df_v2)

# Rename column
df_v2 <- df_v2 %>%
  rename(voxel2 = 'resp_dat$voxel2')

# Sampling
inTrain <- createDataPartition(y = df_v2$voxel2, p = 0.8, list = FALSE)
training <- df_v2[inTrain,]
testing <- df_v2[-inTrain,]

# Lasso
lassoFit2 <- train(voxel2 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 3 --------------------------------------------------------------------

# Getting cor for each
cor_v3 <- data.frame(cor(resp_dat$voxel3, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v3 > 0.2 | cor_v3 < -0.2)

# Select those columns
df_v3 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v3 <- cbind(resp_dat$voxel3, df_v3)

# Rename column
df_v3 <- df_v3 %>%
  rename(voxel3 = 'resp_dat$voxel3')

# Sampling
inTrain <- createDataPartition(y = df_v3$voxel3, p = 0.8, list = FALSE)
training <- df_v3[inTrain,]
testing <- df_v3[-inTrain,]

# Lasso
lassoFit3 <- train(voxel3 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 4 --------------------------------------------------------------------

# Getting cor for each
cor_v4 <- data.frame(cor(resp_dat$voxel4, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v4 > 0.2 | cor_v4 < -0.2)

# Select those columns
df_v4 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v4 <- cbind(resp_dat$voxel4, df_v4)

# Rename column
df_v4 <- df_v4 %>%
  rename(voxel4 = 'resp_dat$voxel4')

# Sampling
inTrain <- createDataPartition(y = df_v4$voxel4, p = 0.8, list = FALSE)
training <- df_v4[inTrain,]
testing <- df_v4[-inTrain,]

# Lasso
lassoFit4 <- train(voxel4 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 5 --------------------------------------------------------------------

# Getting cor for each
cor_v5 <- data.frame(cor(resp_dat$voxel5, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v5 > 0.2 | cor_v5 < -0.2)

# Select those columns
df_v5 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v5 <- cbind(resp_dat$voxel5, df_v5)

# Rename column
df_v5 <- df_v5 %>%
  rename(voxel5 = 'resp_dat$voxel5')

# Sampling
inTrain <- createDataPartition(y = df_v5$voxel5, p = 0.8, list = FALSE)
training <- df_v5[inTrain,]
testing <- df_v5[-inTrain,]

# Lasso
lassoFit5 <- train(voxel5 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 6 --------------------------------------------------------------------

# Getting cor for each
cor_v6 <- data.frame(cor(resp_dat$voxel6, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v6 > 0.2 | cor_v6 < -0.2)

# Select those columns
df_v6 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v6 <- cbind(resp_dat$voxel6, df_v6)

# Rename column
df_v6 <- df_v6 %>%
  rename(voxel6 = 'resp_dat$voxel6')

# Sampling
inTrain <- createDataPartition(y = df_v6$voxel6, p = 0.8, list = FALSE)
training <- df_v6[inTrain,]
testing <- df_v6[-inTrain,]

# Lasso
lassoFit6 <- train(voxel6 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 7 --------------------------------------------------------------------

# Getting cor for each
cor_v7 <- data.frame(cor(resp_dat$voxel7, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v7 > 0.2 | cor_v7 < -0.2)

# Select those columns
df_v7 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v7 <- cbind(resp_dat$voxel7, df_v7)

# Rename column
df_v7 <- df_v7 %>%
  rename(voxel7 = 'resp_dat$voxel7')

# Sampling
inTrain <- createDataPartition(y = df_v7$voxel7, p = 0.8, list = FALSE)
training <- df_v7[inTrain,]
testing <- df_v7[-inTrain,]

# Lasso
lassoFit7 <- train(voxel7 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 8 --------------------------------------------------------------------

# Getting cor for each
cor_v8 <- data.frame(cor(resp_dat$voxel8, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v8 > 0.2 | cor_v8 < -0.2)

# Select those columns
df_v8 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v8 <- cbind(resp_dat$voxel8, df_v8)

# Rename column
df_v8 <- df_v8 %>%
  rename(voxel8 = 'resp_dat$voxel8')

# Sampling
inTrain <- createDataPartition(y = df_v8$voxel8, p = 0.8, list = FALSE)
training <- df_v8[inTrain,]
testing <- df_v8[-inTrain,]

# Lasso
lassoFit8 <- train(voxel8 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 9 --------------------------------------------------------------------

# Getting cor for each
cor_v9 <- data.frame(cor(resp_dat$voxel9, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v9 > 0.2 | cor_v9 < -0.2)

# Select those columns
df_v9 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v9 <- cbind(resp_dat$voxel9, df_v9)

# Rename column
df_v9 <- df_v9 %>%
  rename(voxel9 = 'resp_dat$voxel9')

# Sampling
inTrain <- createDataPartition(y = df_v9$voxel9, p = 0.8, list = FALSE)
training <- df_v9[inTrain,]
testing <- df_v9[-inTrain,]

# Lasso
lassoFit9 <- train(voxel9 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 10 -------------------------------------------------------------------

# Getting cor for each
cor_v10 <- data.frame(cor(resp_dat$voxel10, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v10 > 0.1 | cor_v10 < -0.1)

# Select those columns
df_v10 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v10 <- cbind(resp_dat$voxel10, df_v10)

# Rename column
df_v10 <- df_v10 %>%
  rename(voxel10 = 'resp_dat$voxel10')

# Sampling
inTrain <- createDataPartition(y = df_v10$voxel10, p = 0.8, list = FALSE)
training <- df_v10[inTrain,]
testing <- df_v10[-inTrain,]

# Lasso
lassoFit10 <- train(voxel10 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 11 -------------------------------------------------------------------

# Getting cor for each
cor_v11 <- data.frame(cor(resp_dat$voxel11, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v11 > 0.1 | cor_v11 < -0.1)

# Select those columns
df_v11 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v11 <- cbind(resp_dat$voxel11, df_v11)

# Rename column
df_v11 <- df_v11 %>%
  rename(voxel11 = 'resp_dat$voxel11')

# Sampling
inTrain <- createDataPartition(y = df_v11$voxel11, p = 0.8, list = FALSE)
training <- df_v11[inTrain,]
testing <- df_v11[-inTrain,]

# Lasso
lassoFit11 <- train(voxel11 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 12 -------------------------------------------------------------------

# Getting cor for each
cor_v12 <- data.frame(cor(resp_dat$voxel12, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v12 > 0.2 | cor_v12 < -0.2)

# Select those columns
df_v12 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v12 <- cbind(resp_dat$voxel12, df_v12)

# Rename column
df_v12 <- df_v12 %>%
  rename(voxel12 = 'resp_dat$voxel12')

# Sampling
inTrain <- createDataPartition(y = df_v12$voxel12, p = 0.8, list = FALSE)
training <- df_v12[inTrain,]
testing <- df_v12[-inTrain,]

# Lasso
lassoFit12 <- train(voxel12 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 13 -------------------------------------------------------------------

# Getting cor for each
cor_v13 <- data.frame(cor(resp_dat$voxel13, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v13 > 0.1 | cor_v13 < -0.1)

# Select those columns
df_v13 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v13 <- cbind(resp_dat$voxel13, df_v13)

# Rename column
df_v13 <- df_v13 %>%
  rename(voxel13 = 'resp_dat$voxel13')

# Sampling
inTrain <- createDataPartition(y = df_v13$voxel13, p = 0.8, list = FALSE)
training <- df_v13[inTrain,]
testing <- df_v13[-inTrain,]

# Lasso
lassoFit13 <- train(voxel13 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 14 -------------------------------------------------------------------

# Getting cor for each
cor_v14 <- data.frame(cor(resp_dat$voxel14, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v14 > 0.1 | cor_v14 < -0.1)

# Select those columns
df_v14 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v14 <- cbind(resp_dat$voxel14, df_v14)

# Rename column
df_v14 <- df_v14 %>%
  rename(voxel14 = 'resp_dat$voxel14')

# Sampling
inTrain <- createDataPartition(y = df_v14$voxel14, p = 0.8, list = FALSE)
training <- df_v14[inTrain,]
testing <- df_v14[-inTrain,]

# Lasso
lassoFit14 <- train(voxel14 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 15 -------------------------------------------------------------------

# Getting cor for each
cor_v15 <- data.frame(cor(resp_dat$voxel15, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v15 > 0.2 | cor_v15 < -0.2)

# Select those columns
df_v15 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v15 <- cbind(resp_dat$voxel15, df_v15)

# Rename column
df_v15 <- df_v15 %>%
  rename(voxel15 = 'resp_dat$voxel15')

# Sampling
inTrain <- createDataPartition(y = df_v15$voxel15, p = 0.8, list = FALSE)
training <- df_v15[inTrain,]
testing <- df_v15[-inTrain,]

# Lasso
lassoFit15 <- train(voxel15 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 16 -------------------------------------------------------------------

# Getting cor for each
cor_v16 <- data.frame(cor(resp_dat$voxel16, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v16 > 0.1 | cor_v16 < -0.1)

# Select those columns
df_v16 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v16 <- cbind(resp_dat$voxel16, df_v16)

# Rename column
df_v16 <- df_v16 %>%
  rename(voxel16 = 'resp_dat$voxel16')

# Sampling
inTrain <- createDataPartition(y = df_v16$voxel16, p = 0.8, list = FALSE)
training <- df_v16[inTrain,]
testing <- df_v16[-inTrain,]

# Lasso
lassoFit16 <- train(voxel16 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 17 -------------------------------------------------------------------

# Getting cor for each
cor_v17 <- data.frame(cor(resp_dat$voxel17, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v17 > 0.1 | cor_v17 < -0.1)

# Select those columns
df_v17 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v17 <- cbind(resp_dat$voxel17, df_v17)

# Rename column
df_v17 <- df_v17 %>%
  rename(voxel17 = 'resp_dat$voxel17')

# Sampling
inTrain <- createDataPartition(y = df_v17$voxel17, p = 0.8, list = FALSE)
training <- df_v17[inTrain,]
testing <- df_v17[-inTrain,]

# Lasso
lassoFit17 <- train(voxel17 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 18 -------------------------------------------------------------------

# Getting cor for each
cor_v18 <- data.frame(cor(resp_dat$voxel18, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v18 > 0.2 | cor_v18 < -0.2)

# Select those columns
df_v18 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v18 <- cbind(resp_dat$voxel18, df_v18)

# Rename column
df_v18 <- df_v18 %>%
  rename(voxel18 = 'resp_dat$voxel18')

# Sampling
inTrain <- createDataPartition(y = df_v18$voxel18, p = 0.8, list = FALSE)
training <- df_v18[inTrain,]
testing <- df_v18[-inTrain,]

# Lasso
lassoFit18 <- train(voxel18 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 19 -------------------------------------------------------------------

# Getting cor for each
cor_v19 <- data.frame(cor(resp_dat$voxel19, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v19 > 0.1 | cor_v19 < -0.1)

# Select those columns
df_v19 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v19 <- cbind(resp_dat$voxel19, df_v19)

# Rename column
df_v19 <- df_v19 %>%
  rename(voxel19 = 'resp_dat$voxel19')

# Sampling
inTrain <- createDataPartition(y = df_v19$voxel19, p = 0.8, list = FALSE)
training <- df_v19[inTrain,]
testing <- df_v19[-inTrain,]

# Lasso
lassoFit19 <- train(voxel19 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# Hold for other regression

## Voxel 20 -------------------------------------------------------------------

# Getting cor for each
cor_v20 <- data.frame(cor(resp_dat$voxel20, transformed))

# Grabbing cors higher than 0.2
non_zero_cor <- which(cor_v20 > 0.05 | cor_v20 < -0.05)

# Select those columns
df_v20 <- transformed %>%
  select(non_zero_cor)

# Grabbing database that combines
df_v20 <- cbind(resp_dat$voxel20, df_v20)

# Rename column
df_v20 <- df_v20 %>%
  rename(voxel20 = 'resp_dat$voxel20')

# Sampling
inTrain <- createDataPartition(y = df_v20$voxel20, p = 0.8, list = FALSE)
training <- df_v20[inTrain,]
testing <- df_v20[-inTrain,]

# Lasso
lassoFit20 <- train(voxel20 ~ .,
                   data = training,
                   method = 'rqlasso',
                   trControl = cv_model)

# AIC Test
test20 <- train(voxel20 ~ .,
                    data = training,
                    method = 'glmStepAIC',
                    trControl = cv_model)


## Prediction -----------------------------------------------------------------
prediction <- predict(lassoFit1, newdata = val_feat)
