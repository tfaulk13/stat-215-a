

ggplot(data = all_data, aes(x = temperature)) +
  geom_density(adjust = 1, color = "#c03728") +
  geom_density(adjust = 2, color = "#919c4c") +
  geom_density(adjust = 3, color = "#fd8f24") +
  geom_density(adjust = 4, color = "#828585") +
  geom_density(adjust = 5, color = "#6f5438") +
  scale_color_pomological()
  labs(title = "Gaussian Density Plot for Different Bandwidths",
       x = "Temperature",
       y = "Density") +
  theme_pomological()



ggplot(data = data_morning, aes(y = temperature, x = humidity)) +
  geom_point(color = "#828585") +
  geom_smooth(color = "#c03728") +
  labs(title = "Loess Regression of Temperature on Humidity",
       subtitle = "Measurements taken at 8:00am",
       x = "Humidity",
       y = "Temperature") +
  theme_pomological() +
  theme(legend.position="none")


ggplot(data = all_data, aes(x = temperature)) +
  geom_density(adjust = 0.1, color = "#919c4c") +
  geom_density(adjust = 10, color = "#c03728") +
  labs(title = "Gaussian Density Plot for Extreme Bandwidths",
       subtitle = "Higher bandwidth has more bias; lower bandwidth has more variance", 
       x = "Temperature",
       y = "Density") +
  theme_pomological()


