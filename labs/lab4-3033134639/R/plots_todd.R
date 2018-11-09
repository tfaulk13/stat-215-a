##########################
####### X-Y PLOTS ########
##########################

## Point plots of label for each image ----------------------------------------
ggplot(image_1) + geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_manual(values = c('#00A6FF', '#BFD7EA', '#BCAC8D')) +
  labs(title = 'Image 1') +
  theme_nice 

ggplot(image_2) + geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_manual(values = c('#00A6FF', '#BFD7EA', '#BCAC8D')) +
  labs(title = 'Image 2') +
  gg_theme

ggplot(image_3) + geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_manual(values = c('#00A6FF', '#BFD7EA', '#BCAC8D')) +
  labs(title = 'Image 3') +
  gg_theme

## Point plots of CORR for each image -----------------------------------------
ggplot(image_1) + geom_point(aes(x = x, y = y, color = CORR)) +
  labs(title = 'CORR (1)') +
  gg_theme

ggplot(image_2) + geom_point(aes(x = x, y = y, color = CORR)) +
  labs(title = 'CORR (2)') +
  gg_theme

ggplot(image_3) + geom_point(aes(x = x, y = y, color = CORR)) +
  labs(title = 'CORR (3)') +
  gg_theme

## Point plots of SD for each image -------------------------------------------
ggplot(image_1) + geom_point(aes(x = x, y = y, color = SD)) +
  labs(title = 'SD (1)') +
  gg_theme

ggplot(image_2) + geom_point(aes(x = x, y = y, color = SD)) +
  labs(title = 'SD (2)') +
  gg_theme

ggplot(image_3) + geom_point(aes(x = x, y = y, color = SD)) +
  labs(title = 'SD (3)') +
  gg_theme

## Point plots of NDAI for each image -----------------------------------------
ggplot(image_1) + geom_point(aes(x = x, y = y, color = NDAI)) +
  labs(title = 'NDAI (1)') +
  gg_theme

ggplot(image_2) + geom_point(aes(x = x, y = y, color = NDAI)) +
  labs(title = 'NDAI (2)') +
  gg_theme

ggplot(image_3) + geom_point(aes(x = x, y = y, color = NDAI)) +
  labs(title = 'NDAI (3)') +
  gg_theme

##########################
##### DENSITY PLOTS ######
##########################

## Density graph for NDAI -----------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'NDAI') + 
  theme_nice_wol
ggsave(paste0("densityNDAI.png"))

## Density graph for SD -------------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = SD, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'SD') + 
  xlim(0, 40) +
  theme_nice_woly
ggsave(paste0("densitySD.png"))

## Density graph for CORR -----------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = CORR, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'CORR') + 
  theme_nice_big_ny
ggsave(paste0("densityCORR.png"))
##########################
#### DENSITY PLOTS 2 #####
##########################

## Density graph for AF -------------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = AF, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'AF') + 
  theme_nice_wol
ggsave(paste0("densityAF.png"))
## Density graph for AN -------------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = AN, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'AN') + 
  theme_nice_woly
ggsave(paste0("densityAN.png"))

## Density graph for BF -------------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = BF, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'BF') + 
  theme_nice_big_ny
ggsave(paste0("densityBF.png"))

## Density graph for CF -------------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = CF, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'CF') + 
  theme_nice_wol
ggsave(paste0("densityCF.png"))

## Density graph for DF -------------------------------------------------------
ggplot(images_filter) + 
  geom_density(aes(x = DF, group = factor(label), fill = factor(label)), alpha = 0.5) +
  scale_fill_manual(name = "label", 
                    labels = c("No Cloud", "Cloud"), 
                    values = c("#00A6FF", "#BCAC8D")) + 
  labs(title = 'DF') + 
  theme_nice_big_ny
ggsave(paste0("densityDF.png"))

##########################
###### CORRELATION #######
##########################

## Running correlation --------------------------------------------------------
image_cor <- cor(images_filter[,3:11], images_filter[,3:11])

## Preparing correlation table ------------------------------------------------
upper <- image_cor
upper[upper.tri(image_cor)] <- ""
upper <- as.data.frame(upper)

## Displaying correlation table -----------------------------------------------
upper