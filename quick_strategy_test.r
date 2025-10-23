# Quick strategy comparison test
source("war_car_game_strategy.r")

# Run a quick comparison with fewer games
cat("Running quick strategy comparison with 100 games each...\n")
quick_comparison <- compare_strategies(100)
