# Filtering out ling_data for complete cases ----------------------------------
ling_data <- ling_data[complete.cases(ling_data),]

# Filtering out observations that had incorrect state names -------------------
ling_data <- ling_data %>%
  group_by(STATE) %>%
  filter(n() > 88) %>%
  ungroup()

# Filtering ling_data ---------------------------------------------------------
ling_data <- ling_data %>%
  filter(long > -130)

# Preparing map_df for combining with ling_data -------------------------------
names(map_df) <- c("long", "lat", "group", "order", "state", "county")
map_df$state <- state.abb[match(map_df$state, tolower(state.name))]
map_df$county <- NULL

# Preparing data for maternal grandmother plot --------------------------------
maternal_grandmother <- filter(ling_data, Q068 %in% c(1, 2, 3, 4, 7))
answers_grandmother <- all.ans[['68']]

answers_grandmother$Q068 <- rownames(answers_grandmother)
maternal_grandmother$Q068 <- as.character(maternal_grandmother$Q068)
maternal_grandmother <- inner_join(maternal_grandmother, answers_grandmother, by="Q068")

# Preparing data for maternal grandfather plot --------------------------------
maternal_grandfather <- filter(ling_data, Q070 %in% c(2, 3, 4, 6, 7))
answers_grandfather <- all.ans[['70']]

answers_grandfather$Q070 <- rownames(answers_grandfather)
maternal_grandfather$Q070 <- as.character(maternal_grandfather$Q070)
maternal_grandfather <- inner_join(maternal_grandfather, answers_grandfather, by="Q070")


# Standardizing ling_location -------------------------------------------------
scaled_location <- ling_location

varnames <- c("Number.of.people.in.cell", "Latitude", "Longitude")

# index vector of columns which must not be scaled
index <- names(ling_location) %in% varnames

# scale only the columns not in index
temp <- scale(ling_location[, !index])
scaled_location[, !index] <- temp

 
# K-Means ---------------------------------------------------------------------
k_means <- kmeans(scaled_location, centers = 8)
scaled_location$cluster <- k_means$cluster




