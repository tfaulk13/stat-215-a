## Potential graphs

# Boxplot by date

ggplot(all_data, aes(x = date, y = humidity_adj)) +
  geom_boxplot(fill = 'plum', outlier.alpha = 0.3) +
  theme(axis.text.x = element_text(hjust = 1, angle = 50)) +
  ggtitle('Humidity Appears to Stay Around Same General Level for Multiple Days') +
  xlab('Date') +
  ylab('Relative Humidity') +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(all_data, aes(x = date, y = par_reflected)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(hjust = 1, angle = 50))


# Reordered Boxplot by median

ggplot(all_data, aes(x = reorder(date, humidity, FUN = median), y = humidity)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(hjust = 1, angle = 50))

# Density will fill as tree

ggplot(all_data, aes(x = humidity_adj)) +
  geom_density(aes(fill = tree), alpha = .5)

# Density with fill as direction

ggplot(all_data, aes(x = humidity)) +
  geom_density(aes(fill = direction), alpha = .5)
