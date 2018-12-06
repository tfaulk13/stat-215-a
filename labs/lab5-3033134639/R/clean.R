library(caret)
library(tidyverse)
library(rqPen)



###################################
#####     INITIAL CLEANING    #####
###################################

## Initial transformations ----------------------------------------------------

# Turn resp_dat into a data.frame
resp_dat <- data.frame(resp_dat)
names(resp_dat) <- paste("voxel", 1:ncol(resp_dat), sep = "")

# Turn fit_feat into a data.frame
fit_feat <- data.frame(fit_feat)

# Turn val_feat into a data.frame
val_feat <- data.frame(val_feat)

# Turn loc_dat into a data.frame
loc_dat <- data.frame(loc_dat)


## Preprocessing --------------------------------------------------------------

# process 
processed <- preProcess(fit_feat, method = c('center', 'scale'))

# Turning processed data into a dataframe
transformed <- predict(processed, newdata = fit_feat)


