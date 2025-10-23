# Simplified ML-based War Strategy Optimization
# Uses basic statistical learning without external packages

# Basic game functions
deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  values <- c(2:10, "J", "Q", "K", "A")
  cards <- expand.grid(Value = values, Suit = suits)
  cards$Rank <- match(cards$Value, c(2:10, "J", "Q", "K", "A"))
  return(cards)
}

shuffle_deck <- function(deck) {
  deck[sample(nrow(deck)), ]
}

deal_cards <- function(shuffled_deck) {
  player1 <- shuffled_deck[1:26, ]
  player2 <- shuffled_deck[27:52, ]
  list(player1 = player1, player2 = player2)
}

# Feature extraction for decision making
extract_game_state <- function(p2_hand, p1_hand, p2_card) {
  total_cards <- nrow(p2_hand) + nrow(p1_hand)
  p2_cards <- nrow(p2_hand)
  p1_cards <- nrow(p1_hand)
  
  list(
    card_ratio = p2_cards / total_cards,           # 0.0 to 1.0
    card_advantage = p2_cards - p1_cards,         # -26 to +26
    current_strength = p2_card$Rank / 13,         # 0.077 to 1.0
    is_behind = p2_cards < p1_cards,              # TRUE/FALSE
    is_far_behind = p2_cards < (p1_cards * 0.7),  # TRUE/FALSE
    aces_p2 = sum(p2_hand$Value == "A"),          # 0 to 4
    aces_p1 = sum(p1_hand$Value == "A"),          # 0 to 4
    high_cards_p2 = sum(p2_hand$Rank >= 10),      # 0 to 20
    game_progress = 1 - (total_cards / 52)        # 0.0 to 1.0
  )
}

# Simulate a game with forfeit strategy function
simulate_war_game <- function(forfeit_strategy = NULL, verbose = FALSE) {
  d <- deck()
  shuffled <- shuffle_deck(d)
  hands <- deal_cards(shuffled)
  player1 <- hands$player1
  player2 <- hands$player2
  
  rounds <- 0
  p2_forfeits <- 0
  decisions <- list()
  
  while (nrow(player1) > 0 && nrow(player2) > 0 && rounds < 1000) {
    rounds <- rounds + 1
    
    card1 <- player1[1, ]
    card2 <- player2[1, ]
    player1 <- player1[-1, ]
    player2 <- player2[-1, ]
    
    # Extract game state for decision
    state <- extract_game_state(player2, player1, card2)
    
    # Player 2 forfeit decision (only for non-ties)
    should_forfeit <- FALSE
    if (!is.null(forfeit_strategy) && card1$Rank != card2$Rank) {
      should_forfeit <- forfeit_strategy(state)
      if (should_forfeit) p2_forfeits <- p2_forfeits + 1
    }
    
    # Determine round outcome
    round_outcome <- NA
    if (should_forfeit) {
      round_outcome <- "forfeit"
      player1 <- rbind(player1, card1, card2)
      if (verbose) cat("Round", rounds, ": P2 forfeits", card2$Value, "vs", card1$Value, "\n")
    } else if (card1$Rank > card2$Rank) {
      round_outcome <- "lose"
      player1 <- rbind(player1, card1, card2)
      if (verbose) cat("Round", rounds, ": P2 loses", card2$Value, "vs", card1$Value, "\n")
    } else if (card1$Rank < card2$Rank) {
      round_outcome <- "win"
      player2 <- rbind(player2, card2, card1)
      if (verbose) cat("Round", rounds, ": P2 wins", card2$Value, "vs", card1$Value, "\n")
    } else {
      # War
      round_outcome <- "war"
      if (verbose) cat("Round", rounds, ": WAR!\n")
      
      if (nrow(player1) >= 4 && nrow(player2) >= 4) {
        war_cards1 <- player1[1:4, ]
        war_cards2 <- player2[1:4, ]
        player1 <- player1[-(1:4), ]
        player2 <- player2[-(1:4), ]
        
        if (war_cards1[4, ]$Rank > war_cards2[4, ]$Rank) {
          player1 <- rbind(player1, card1, card2, war_cards1, war_cards2)
        } else if (war_cards1[4, ]$Rank < war_cards2[4, ]$Rank) {
          player2 <- rbind(player2, card1, card2, war_cards1, war_cards2)
        } else {
          player1 <- rbind(player1, card1, war_cards1)
          player2 <- rbind(player2, card2, war_cards2)
        }
      } else {
        break
      }
    }
    
    # Store decision data
    decisions[[rounds]] <- list(
      state = state,
      decision = should_forfeit,
      outcome = round_outcome
    )
  }
  
  winner <- ifelse(nrow(player1) > nrow(player2), 1, 
                  ifelse(nrow(player2) > nrow(player1), 2, 0))
  
  return(list(
    winner = winner,
    p2_won = winner == 2,
    rounds = rounds,
    forfeits = p2_forfeits,
    decisions = decisions
  ))
}

# Generate training data using random strategies
generate_learning_data <- function(n_games = 1000) {
  cat("Generating learning data from", n_games, "games...\n")
  
  all_decisions <- list()
  decision_count <- 0
  
  for (game in 1:n_games) {
    if (game %% 200 == 0) cat("Game", game, "of", n_games, "\n")
    
    # Random forfeit probability for this game
    forfeit_prob <- runif(1, 0, 0.4)
    
    # Random strategy
    random_strategy <- function(state) {
      prob <- forfeit_prob
      
      # Adjust probability based on situation
      if (state$is_behind) prob <- prob * 1.5
      if (state$current_strength < 0.4) prob <- prob * 1.3
      if (state$aces_p2 < 2) prob <- prob * 1.2
      
      return(runif(1) < prob)
    }
    
    # Simulate game
    result <- simulate_war_game(random_strategy)
    
    # Extract decision outcomes
    for (decision in result$decisions) {
      if (decision$outcome %in% c("win", "lose", "forfeit")) {
        decision_count <- decision_count + 1
        
        immediate_value <- switch(decision$outcome,
          "win" = 1, "lose" = -1, "forfeit" = -1)
        
        game_value <- ifelse(result$p2_won, 1, -1)
        combined_value <- 0.2 * immediate_value + 0.8 * game_value
        
        all_decisions[[decision_count]] <- list(
          state = decision$state,
          forfeited = decision$decision,
          immediate_value = immediate_value,
          game_value = game_value,
          combined_value = combined_value
        )
      }
    }
  }
  
  cat("Generated", length(all_decisions), "decision points\n")
  return(all_decisions)
}

# Simple decision tree learning
learn_forfeit_strategy <- function(decision_data) {
  cat("Learning optimal forfeit strategy...\n")
  
  # Extract features and outcomes
  n <- length(decision_data)
  
  # Create feature matrix
  features <- data.frame(
    card_ratio = sapply(decision_data, function(x) x$state$card_ratio),
    card_advantage = sapply(decision_data, function(x) x$state$card_advantage),
    current_strength = sapply(decision_data, function(x) x$state$current_strength),
    is_behind = sapply(decision_data, function(x) as.numeric(x$state$is_behind)),
    is_far_behind = sapply(decision_data, function(x) as.numeric(x$state$is_far_behind)),
    aces_p2 = sapply(decision_data, function(x) x$state$aces_p2),
    combined_value = sapply(decision_data, function(x) x$combined_value),
    forfeited = sapply(decision_data, function(x) x$forfeited)
  )
  
  # Learn simple rules by analyzing when forfeiting leads to better outcomes
  
  # Rule 1: Card strength thresholds
  strength_thresholds <- seq(0.1, 0.9, 0.1)
  best_strength_threshold <- 0.5
  best_strength_score <- -Inf
  
  for (threshold in strength_thresholds) {
    forfeit_mask <- features$current_strength <= threshold
    if (sum(forfeit_mask) > 50) {  # Need enough samples
      avg_value <- mean(features$combined_value[forfeit_mask])
      if (avg_value > best_strength_score) {
        best_strength_score <- avg_value
        best_strength_threshold <- threshold
      }
    }
  }
  
  # Rule 2: Card ratio thresholds
  ratio_thresholds <- seq(0.2, 0.8, 0.1)
  best_ratio_threshold <- 0.4
  best_ratio_score <- -Inf
  
  for (threshold in ratio_thresholds) {
    forfeit_mask <- features$card_ratio <= threshold & features$current_strength <= 0.6
    if (sum(forfeit_mask) > 50) {
      avg_value <- mean(features$combined_value[forfeit_mask])
      if (avg_value > best_ratio_score) {
        best_ratio_score <- avg_value
        best_ratio_threshold <- threshold
      }
    }
  }
  
  # Rule 3: Behind + weak card combinations
  behind_weak_mask <- features$is_behind == 1 & features$current_strength <= 0.5
  behind_weak_value <- ifelse(sum(behind_weak_mask) > 20, 
                             mean(features$combined_value[behind_weak_mask]), -1)
  
  far_behind_mask <- features$is_far_behind == 1 & features$current_strength <= 0.7
  far_behind_value <- ifelse(sum(far_behind_mask) > 10,
                            mean(features$combined_value[far_behind_mask]), -1)
  
  cat("Learned strategy parameters:\n")
  cat("  Best card strength threshold:", best_strength_threshold, "(score:", round(best_strength_score, 3), ")\n")
  cat("  Best card ratio threshold:", best_ratio_threshold, "(score:", round(best_ratio_score, 3), ")\n")
  cat("  Behind + weak card value:", round(behind_weak_value, 3), "\n")
  cat("  Far behind + medium card value:", round(far_behind_value, 3), "\n")
  
  # Create learned strategy function
  learned_strategy <- function(state) {
    # Multi-condition strategy based on learned thresholds
    
    # Primary: Very weak cards when behind
    if (state$is_behind && state$current_strength <= best_strength_threshold) {
      return(TRUE)
    }
    
    # Secondary: Weak cards when card ratio is poor
    if (state$card_ratio <= best_ratio_threshold && state$current_strength <= 0.6) {
      return(TRUE)
    }
    
    # Tertiary: Medium cards when far behind
    if (state$is_far_behind && state$current_strength <= 0.7) {
      return(TRUE)
    }
    
    # Quaternary: Very weak cards with few aces
    if (state$aces_p2 <= 1 && state$current_strength <= 0.3) {
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  return(list(
    strategy = learned_strategy,
    params = list(
      strength_threshold = best_strength_threshold,
      ratio_threshold = best_ratio_threshold,
      behind_weak_value = behind_weak_value,
      far_behind_value = far_behind_value
    )
  ))
}

# Test different strategies
test_strategy_performance <- function(strategy_list, n_games = 500) {
  cat("\n=== TESTING STRATEGY PERFORMANCE ===\n")
  
  results <- list()
  
  for (name in names(strategy_list)) {
    cat("Testing", name, "...\n")
    
    wins <- 0
    forfeits <- 0
    total_rounds <- 0
    
    set.seed(123)  # Consistent testing
    for (i in 1:n_games) {
      game_result <- simulate_war_game(strategy_list[[name]])
      if (game_result$winner == 2) wins <- wins + 1
      forfeits <- forfeits + game_result$forfeits
      total_rounds <- total_rounds + game_result$rounds
    }
    
    win_rate <- wins / n_games * 100
    forfeit_rate <- forfeits / total_rounds * 100
    
    results[[name]] <- list(
      win_rate = win_rate,
      wins = wins,
      games = n_games,
      forfeit_rate = forfeit_rate
    )
    
    cat("  Win rate:", round(win_rate, 1), "% | Forfeit rate:", round(forfeit_rate, 1), "%\n")
  }
  
  return(results)
}

# Main execution
cat("=== WAR GAME ML STRATEGY OPTIMIZATION ===\n")
cat("==========================================\n\n")

# Step 1: Generate training data
training_data <- generate_learning_data(1500)

# Step 2: Learn strategy
learned_model <- learn_forfeit_strategy(training_data)
ml_strategy <- learned_model$strategy

# Step 3: Define baseline strategies
strategies <- list(
  "No Forfeit" = NULL,
  "Always Forfeit Low" = function(s) s$current_strength <= 0.46,  # Cards 2-6
  "Forfeit When Behind" = function(s) s$is_behind && s$current_strength <= 0.54,  # Behind + cards 2-7
  "Conservative" = function(s) s$is_behind && s$current_strength <= 0.38 && s$aces_p2 < 2,
  "Aggressive" = function(s) s$current_strength <= 0.61 && (s$is_behind || s$aces_p2 < 2),
  "ML Learned" = ml_strategy
)

# Step 4: Test all strategies
results <- test_strategy_performance(strategies, 1000)

# Step 5: Analyze results
cat("\n=== FINAL RESULTS COMPARISON ===\n")
cat("=================================\n")

# Find best strategy
best_name <- names(results)[which.max(sapply(results, function(x) x$win_rate))]
baseline_rate <- results[["No Forfeit"]]$win_rate
ml_rate <- results[["ML Learned"]]$win_rate

for (name in names(results)) {
  r <- results[[name]]
  marker <- ifelse(name == best_name, " *** BEST ***", "")
  improvement <- r$win_rate - baseline_rate
  
  cat(sprintf("%-20s: %5.1f%% (%+.1f) | Forfeit: %4.1f%% %s\n", 
              name, r$win_rate, improvement, r$forfeit_rate, marker))
}

cat("\n=== ML STRATEGY ANALYSIS ===\n")
improvement <- ml_rate - baseline_rate
cat("Baseline win rate:", round(baseline_rate, 1), "%\n")
cat("ML strategy win rate:", round(ml_rate, 1), "%\n")
cat("ML improvement:", sprintf("%+.1f", improvement), "percentage points\n")

if (improvement > 2) {
  cat("✓ SIGNIFICANT IMPROVEMENT: ML strategy substantially helps Player 2!\n")
} else if (improvement > 0.5) {
  cat("✓ MODERATE IMPROVEMENT: ML strategy helps Player 2\n")
} else if (improvement > -0.5) {
  cat("○ MINIMAL IMPACT: ML strategy has little effect\n")
} else {
  cat("✗ NEGATIVE IMPACT: ML strategy hurts Player 2\n")
}

cat("\nML Strategy Parameters:\n")
for (param in names(learned_model$params)) {
  cat(" ", param, ":", learned_model$params[[param]], "\n")
}

# Test a few games with detailed output
cat("\n=== SAMPLE ML STRATEGY GAMES ===\n")
for (i in 1:3) {
  cat("\nSample Game", i, ":\n")
  set.seed(100 + i)
  sample_result <- simulate_war_game(ml_strategy, verbose = TRUE)
  cat("Result: Player", sample_result$winner, "wins after", sample_result$rounds, 
      "rounds (", sample_result$forfeits, "forfeits)\n")
}

cat("\n=== CONCLUSION ===\n")
if (best_name == "ML Learned") {
  cat("🏆 The ML-learned strategy is the BEST performer!\n")
  cat("The machine learning approach successfully discovered an optimal forfeit strategy.\n")
} else {
  cat("🎯 Best strategy:", best_name, "\n")
  cat("The ML strategy ranked #", which(names(results) == "ML Learned"), "out of", length(results), "\n")
}

cat("\nKey findings:\n")
cat("- Optimal forfeit rate: ~", round(results[["ML Learned"]]$forfeit_rate, 1), "% of rounds\n")
cat("- Performance gain over baseline: ", sprintf("%+.1f", improvement), " percentage points\n")
cat("- Strategy learned from", length(training_data), "decision examples\n")
