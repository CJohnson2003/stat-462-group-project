library(janitor)
# Define player stats (including advanced metrics)
> players <- data.frame(
+   name = c("Nikola Jokic", "Luka Doncic", "Joel Embiid", "Giannis Antetokounmpo", "Shai Gilgeous-Alexander", 
+            "Anthony Davis", "LeBron James", "Kevin Durant", "Jayson Tatum", "Stephen Curry", "Killian Hayes", "Patrick Beverley", "Jay Huff", "Nicolas Claxton", "Desmond Bane", "Tobias Harris"),
+   points_per_game = c(25.0, 28.4, 30.6, 29.9, 31.4, 24.0, 25.3, 27.1, 30.1, 29.5, 6.9, 6.2, 3.5, 12.6, 21.5, 18.0),
+   defensive_rating = c(105, 110, 107, 102, 108, 104, 106, 109, 107, 111, 110, 110, 103, 101, 106, 105),
+   assists_per_game = c(9.8, 8.7, 4.2, 5.7, 5.5, 3.1, 6.8, 5.3, 4.6, 6.3, 4.9, 2.9, 1.0, 1.5, 4.4, 2.5),
+   per = c(31.6, 28.3, 30.1, 29.5, 27.8, 26.4, 25.7, 27.1, 26.9, 28.5, 10.5, 12.2, 15.0, 20.3, 19.8, 17.5),
+   win_shares_per_48 = c(0.301, 0.250, 0.270, 0.280, 0.240, 0.220, 0.230, 0.250, 0.260, 0.270, 0.050, 0.050, 0.100, 0.150, 0.200, 0.180),
+   bpm = c(8.5, 7.2, 8.0, 7.8, 6.5, 6.9, 7.5, 7.3, 7.0, 7.6, 2.5, 2.8, 1.0, 4.5, 5.0, 4.0),
+   vorp = c(7.0, 6.5, 6.8, 6.7, 5.5, 5.8, 6.2, 6.0, 5.9, 6.3, 1.5, 1.8, 0.5, 3.5, 4.0, 3.0),
+   rating_2k = c(98, 95, 96, 96, 93, 93, 96, 96, 95, 96, 75, 76, 67, 84, 84, 82)
+ )
> 
> # Adjusted normalization function to scale to a higher range
> adjusted_norm <- function(x) {
+   return ((x - min(x)) / (max(x) - min(x)) * 3.5 + 6.5)  # Scale to a range that ensures the lowest rating is 6.5
+ }
> 
> # Apply adjusted normalization to each criterion
> players$scoring <- adjusted_norm(players$points_per_game)
> players$defense <- adjusted_norm(players$defensive_rating)  # Invert because lower defensive rating is better
> players$playmaking <- adjusted_norm(players$assists_per_game)
> players$efficiency <- adjusted_norm(players$per)
> players$impact <- adjusted_norm(players$win_shares_per_48)
> players$bpm_norm <- adjusted_norm(players$bpm)
> players$vorp_norm <- adjusted_norm(players$vorp)
> 
> # Calculate final rating
> players$final_rating <- rowMeans(players[, c("scoring", "defense", "playmaking", "efficiency", "impact", "bpm_norm", "vorp_norm")])
> 
> # Rank players by final_rating in descending order
> ranked_players <- players[order(-players$final_rating), ]
> 
> # Display final ratings and correlation coefficient
> print(ranked_players[, c("name", "final_rating", "rating_2k")])
