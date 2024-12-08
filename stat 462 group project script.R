# Define player stats (example values)
players <- data.frame(
  name = c("Nikola Jokic", "Luka Doncic", "Joel Embiid", "Giannis Antetokounmpo", "Shai Gilgeous-Alexander", 
           "Anthony Davis", "LeBron James", "Kevin Durant", "Jayson Tatum", "Stephen Curry"),
  points_per_game = c(25.0, 28.4, 30.6, 29.9, 31.4, 24.0, 25.3, 27.1, 30.1, 29.5),
  defensive_rating = c(105, 110, 107, 102, 108, 104, 106, 109, 107, 111),
  assists_per_game = c(9.8, 8.7, 4.2, 5.7, 5.5, 3.1, 6.8, 5.3, 4.6, 6.3),
  per = c(31.6, 28.3, 30.1, 29.5, 27.8, 26.4, 25.7, 27.1, 26.9, 28.5),
  win_shares_per_48 = c(0.301, 0.250, 0.270, 0.280, 0.240, 0.220, 0.230, 0.250, 0.260, 0.270)
)

# Normalize stats to a 10-point scale
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)) * 10)
}

players$scoring <- normalize(players$points_per_game)
players$defense <- normalize(players$defensive_rating)
players$playmaking <- normalize(players$assists_per_game)
players$efficiency <- normalize(players$per)
players$impact <- normalize(players$win_shares_per_48)

# Calculate final rating
players$final_rating <- rowMeans(players[, c("scoring", "defense", "playmaking", "efficiency", "impact")])

# Display final ratings
players[, c("name", "final_rating")]
