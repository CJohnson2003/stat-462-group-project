# Load necessary library
library(glmnet)

# Define player stats (including Paolo Banchero, Myles Turner, Austin Reaves, and D'Angelo Russell)
players <- data.frame(
  name = c("Nikola Jokic", "Luka Doncic", "Joel Embiid", "Giannis Antetokounmpo", "Shai Gilgeous-Alexander", 
           "Anthony Davis", "LeBron James", "Kevin Durant", "Jayson Tatum", "Stephen Curry", "Killian Hayes", 
           "Patrick Beverley", "Jay Huff", "Nicolas Claxton", "Desmond Bane", "Tobias Harris", "Paolo Banchero", 
           "Myles Turner", "Austin Reaves", "D'Angelo Russell"),
  points_per_game = c(25.0, 28.4, 30.6, 29.9, 31.4, 24.0, 25.3, 27.1, 30.1, 29.5, 6.9, 6.2, 3.5, 12.6, 21.5, 18.0, 
                      20.0, 15.0, 13.0, 17.0),
  defensive_rating = c(105, 110, 107, 102, 108, 104, 106, 109, 107, 111, 110, 110, 103, 101, 106, 105, 
                       108, 103, 107, 108),
  assists_per_game = c(9.8, 8.7, 4.2, 5.7, 5.5, 3.1, 6.8, 5.3, 4.6, 6.3, 4.9, 2.9, 1.0, 1.5, 4.4, 2.5, 
                       3.7, 1.2, 3.4, 6.1),
  per = c(31.6, 28.3, 30.1, 29.5, 27.8, 26.4, 25.7, 27.1, 26.9, 28.5, 10.5, 12.2, 15.0, 20.3, 19.8, 17.5, 
          16.0, 18.0, 14.0, 16.5),
  win_shares_per_48 = c(0.301, 0.250, 0.270, 0.280, 0.240, 0.220, 0.230, 0.250, 0.260, 0.270, 0.050, 0.050, 
                        0.100, 0.150, 0.200, 0.180, 0.150, 0.200, 0.120, 0.140),
  bpm = c(8.5, 7.2, 8.0, 7.8, 6.5, 6.9, 7.5, 7.3, 7.0, 7.6, 2.5, 2.8, 1.0, 4.5, 5.0, 4.0, 3.8, 4.2, 3.2, 4.1),
  vorp = c(7.0, 6.5, 6.8, 6.7, 5.5, 5.8, 6.2, 6.0, 5.9, 6.3, 1.5, 1.8, 0.5, 3.5, 4.0, 3.0, 3.5, 4.0, 3.0, 3.5),
  rating_2k = c(98, 95, 96, 96, 93, 93, 96, 96, 95, 96, 75, 76, 67, 84, 84, 82, 84, 83, 80, 83)  # Provided 2K ratings
)

# Adjusted normalization function to scale to a higher range
adjusted_norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)) * 3.5 + 6.5)  # Scale to a range that ensures the lowest rating is 6.5
}

# Apply adjusted normalization to each criterion
players$scoring <- adjusted_norm(players$points_per_game)
players$defense <- adjusted_norm(players$defensive_rating)  # Invert because lower defensive rating is better
players$playmaking <- adjusted_norm(players$assists_per_game)
players$efficiency <- adjusted_norm(players$per)
players$impact <- adjusted_norm(players$win_shares_per_48)
players$bpm_norm <- adjusted_norm(players$bpm)
players$vorp_norm <- adjusted_norm(players$vorp)

# Calculate final rating using your formula
players$final_rating_formula <- rowMeans(players[, c("scoring", "defense", "playmaking", "efficiency", "impact")])

# Prepare data for ridge regression
x <- as.matrix(players[, c("points_per_game", "defensive_rating", "assists_per_game", "per", "win_shares_per_48", "bpm", "vorp")])
y <- players$rating_2k

# Fit ridge regression model
ridge_model <- cv.glmnet(x, y, alpha = 0)

# Predict ratings using the ridge regression model
players$predicted_rating_ridge <- predict(ridge_model, s = "lambda.min", newx = x)

# Calculate correlation coefficient between predicted_rating and 2K rating
correlation_coefficient_formula <- cor(players$final_rating_formula, players$rating_2k)
correlation_coefficient_ridge <- cor(players$predicted_rating_ridge, players$rating_2k)

# Rank players by final_rating in descending order
ranked_players_formula <- players[order(-players$final_rating_formula), ]
ranked_players_ridge <- players[order(-players$predicted_rating_ridge), ]

# Display final ratings and correlation coefficients
print("Ranked Players by Formula-Based Ratings:")
print(ranked_players_formula[, c("name", "final_rating_formula", "rating_2k")])
print(paste("The correlation coefficient between the formula ratings and the actual ratings is", correlation_coefficient_formula))

print("\nRanked Players by Ridge Regression-Based Ratings:")
print(ranked_players_ridge[, c("name", "predicted_rating_ridge", "rating_2k")])
print(paste("The correlation coefficient between the ridge regression ratings and the actual ratings is", correlation_coefficient_ridge))library(janitor)
