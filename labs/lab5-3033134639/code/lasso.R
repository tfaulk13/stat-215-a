getLasso <- function(df, voxel_data, train_model, voxel_num) {
  voxel_name <- paste0('voxel', voxel_num)
  
 # Need name of voxel df
  
  cor_vox <- data.frame(cor(voxel_data$voxel_name, df))
  
  # Grabbing cors higher than 0.2
  non_zero_cor <- which(cor_vox > 0.2 | cor_vox < -0.2)
  
  # Select those columns
  df_vox <- df %>%
    select(non_zero_cor)
  
  # Grabbing database that combines
  df_vox <- cbind(resp_dat$voxel1, df_vox)
  
  # Rename column
  df_vox <- df_vox %>%
    rename(voxel = voxel_name)
  
  inTrain <- createDataPartition(y = df_vox$voxel, p = 0.8, list = FALSE)
  training <- df_vox[inTrain,]
  testing <- df_vox[-inTrain,]
  

  
  lassoFit1 <- train(voxel ~ .,
                        data = training,
                        method = 'rqlasso',
                        trControl = train_model)
  
  lassoFit
}



