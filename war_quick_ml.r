# Quick ML Strategy Test for War Card Game
cat("=== WAR GAME ML FORFEIT STRATEGY ===\n")

# Basic game setup
deck <- function() {
  suits <- rep(c("H", "D", "C", "S"), each = 13)
  values <- rep(2:14, 4)  # 2-10, J=11, Q=12, K=13, A=14
  data.frame(suit = suits, rank = values)
}

# Simple game simulation
play_war <- function(forfeit_func = NULL) {
  cards <- deck()[sample(52), ]
  p1_cards <- cards[1:26, ]
  p2_cards <- cards[27:52, ]
  
  rounds <- 0
  forfeits <- 0
  
  while (nrow(p1_cards) > 0 && nrow(p2_cards) > 0 && rounds < 500) {
    rounds <- rounds + 1
    
    c1 <- p1_cards[1, ]
    c2 <- p2_cards[1, ]
    p1_cards <- p1_cards[-1, , drop = FALSE]
    p2_cards <- p2_cards[-1, , drop = FALSE]
    
    # Game state for ML decision
    state <- list(
      p2_ratio = nrow(p2_cards) / (nrow(p1_cards) + nrow(p2_cards)),
      card_strength = c2$rank / 14,
      is_behind = nrow(p2_cards) < nrow(p1_cards),
      p2_aces = sum(p2_cards$rank == 14)
    )
    
    # Forfeit decision (Player 2 only, no ties)
    forfeit <- FALSE
    if (!is.null(forfeit_func) && c1$rank != c2$rank) {
      forfeit <- forfeit_func(state)
      if (forfeit) forfeits <- forfeits + 1
    }
    
    # Round outcome
    if (forfeit) {
      p1_cards <- rbind(p1_cards, c1, c2)
    } else if (c1$rank > c2$rank) {
      p1_cards <- rbind(p1_cards, c1, c2)
    } else if (c2$rank > c1$rank) {
      p2_cards <- rbind(p2_cards, c2, c1)
    } else {
      # Simplified war
      if (nrow(p1_cards) >= 2 && nrow(p2_cards) >= 2) {
        w1 <- p1_cards[1:2, ]
        w2 <- p2_cards[1:2, ]
        p1_cards <- p1_cards[-(1:2), , drop = FALSE]
        p2_cards <- p2_cards[-(1:2), , drop = FALSE]
        
        if (w1[2, ]$rank > w2[2, ]$rank) {
          p1_cards <- rbind(p1_cards, c1, c2, w1, w2)
        } else {
          p2_cards <- rbind(p2_cards, c2, c1, w2, w1)
        }
      }
    }
  }
  
  winner <- ifelse(nrow(p1_cards) > nrow(p2_cards), 1, 2)
  return(list(winner = winner, rounds = rounds, forfeits = forfeits))
}

# Test strategies
test_strategy <- function(name, strategy_func, n_games = 500) {
  wins <- 0
  total_forfeits <- 0
  total_rounds <- 0
  
  set.seed(42)
  for (i in 1:n_games) {
    result <- play_war(strategy_func)
    if (result$winner == 2) wins <- wins + 1
    total_forfeits <- total_forfeits + result$forfeits
    total_rounds <- total_rounds + result$rounds
  }
  
  win_rate <- wins / n_games * 100
  forfeit_rate <- ifelse(total_rounds > 0, total_forfeits / total_rounds * 100, 0)
  
  return(list(name = name, win_rate = win_rate, forfeit_rate = forfeit_rate))
}

# Generate training examples
cat("Generating training data...\n")
training_examples <- list()
example_count <- 0

# Collect 5000 decision examples using random strategies
set.seed(123)
for (game in 1:1000) {
  forfeit_prob <- runif(1, 0, 0.3)
  
  random_strategy <- function(state) {
    prob <- forfeit_prob
    if (state$is_behind) prob <- prob * 1.4
    if (state$card_strength < 0.5) prob <- prob * 1.3
    return(runif(1) < prob)
  }
  
  # Modified play_war to collect decision data
  cards <- deck()[sample(52), ]
  p1_cards <- cards[1:26, ]
  p2_cards <- cards[27:52, ]
  
  rounds <- 0
  game_decisions <- list()
  
  while (nrow(p1_cards) > 0 && nrow(p2_cards) > 0 && rounds < 200) {
    rounds <- rounds + 1
    
    c1 <- p1_cards[1, ]
    c2 <- p2_cards[1, ]
    p1_cards <- p1_cards[-1, , drop = FALSE]
    p2_cards <- p2_cards[-1, , drop = FALSE]
    
    if (c1$rank != c2$rank) {  # Only non-tie decisions
      state <- list(
        p2_ratio = nrow(p2_cards) / (nrow(p1_cards) + nrow(p2_cards)),
        card_strength = c2$rank / 14,
        is_behind = nrow(p2_cards) < nrow(p1_cards),
        p2_aces = sum(p2_cards$rank == 14)
      )
      
      forfeit <- random_strategy(state)
      outcome <- ifelse(forfeit, -1, ifelse(c2$rank > c1$rank, 1, -1))
      
      game_decisions[[length(game_decisions) + 1]] <- list(
        state = state, decision = forfeit, outcome = outcome
      )
    }
    
    # Apply decision
    forfeit <- ifelse(length(game_decisions) > 0, 
                     tail(game_decisions, 1)[[1]]$decision, FALSE)
    
    if (forfeit && c1$rank != c2$rank) {
      p1_cards <- rbind(p1_cards, c1, c2)
    } else if (c1$rank > c2$rank) {
      p1_cards <- rbind(p1_cards, c1, c2)
    } else if (c2$rank > c1$rank) {
      p2_cards <- rbind(p2_cards, c2, c1)
    }
  }
  
  # Determine game outcome
  p2_won <- nrow(p2_cards) > nrow(p1_cards)
  game_value <- ifelse(p2_won, 1, -1)
  
  # Store decisions with game context
  for (decision in game_decisions) {
    example_count <- example_count + 1
    training_examples[[example_count]] <- list(
      state = decision$state,
      decision = decision$decision,
      immediate = decision$outcome,
      game_result = game_value,
      combined = 0.3 * decision$outcome + 0.7 * game_value
    )
  }
}

cat("Collected", length(training_examples), "training examples\n")

# Simple ML: Learn optimal thresholds
cat("Learning optimal strategy...\n")

# Test different threshold combinations
best_combo <- NULL
best_score <- -Inf

thresholds <- expand.grid(
  strength_thresh = c(0.3, 0.4, 0.5, 0.6),
  ratio_thresh = c(0.3, 0.4, 0.5),
  behind_factor = c(TRUE, FALSE)
)

for (i in seq_len(nrow(thresholds))) {
  thresh <- thresholds[i, ]
  
  # Apply this threshold combination to training data
  predicted_forfeits <- sapply(training_examples, function(ex) {
    s <- ex$state
    should_forfeit <- (s$card_strength <= thresh$strength_thresh) &&
                     (s$p2_ratio <= thresh$ratio_thresh || !thresh$behind_factor) &&
                     (!thresh$behind_factor || s$is_behind)
    return(should_forfeit)
  })
  
  # Calculate score (average combined value when following this strategy)
  score <- mean(sapply(seq_along(training_examples), function(j) {
    ex <- training_examples[[j]]
    if (predicted_forfeits[j]) {
      # If we predict forfeit, use forfeit outcome
      return(ex$combined)  # This is already the combined outcome
    } else {
      # If we predict no forfeit, use non-forfeit outcome  
      return(ex$immediate * 0.3 + ex$game_result * 0.7)
    }
  }))
  
  if (score > best_score) {
    best_score <- score
    best_combo <- thresh
  }
}

cat("Best learned parameters:\n")
cat("  Card strength threshold:", best_combo$strength_thresh, "\n")
cat("  Card ratio threshold:", best_combo$ratio_thresh, "\n")  
cat("  Require behind:", best_combo$behind_factor, "\n")
cat("  Score:", round(best_score, 3), "\n\n")

# Create ML strategy
ml_strategy <- function(state) {
  return((state$card_strength <= best_combo$strength_thresh) &&
         (state$p2_ratio <= best_combo$ratio_thresh || !best_combo$behind_factor) &&
         (!best_combo$behind_factor || state$is_behind))
}

# Test all strategies
cat("Testing strategies...\n")

strategies <- list(
  list(name = "No Forfeit", func = NULL),
  list(name = "Forfeit Low Cards", func = function(s) s$card_strength <= 0.43),
  list(name = "Forfeit When Behind", func = function(s) s$is_behind && s$card_strength <= 0.5),
  list(name = "Conservative", func = function(s) s$is_behind && s$card_strength <= 0.36),
  list(name = "ML Learned", func = ml_strategy)
)

results <- list()
for (strat in strategies) {
  result <- test_strategy(strat$name, strat$func, 1000)
  results[[length(results) + 1]] <- result
  cat(sprintf("%-18s: %5.1f%% win rate, %4.1f%% forfeit rate\n", 
              result$name, result$win_rate, result$forfeit_rate))
}

# Analyze results
cat("\n=== RESULTS ANALYSIS ===\n")
baseline <- results[[1]]$win_rate
ml_result <- results[[length(results)]]$win_rate
improvement <- ml_result - baseline

cat("Baseline (no forfeit):", round(baseline, 1), "%\n")
cat("ML strategy:", round(ml_result, 1), "%\n") 
cat("Improvement:", sprintf("%+.1f", improvement), "percentage points\n\n")

if (improvement > 1) {
  cat("✓ SUCCESS: ML strategy significantly improves Player 2's win rate!\n")
} else if (improvement > 0) {
  cat("✓ MODEST SUCCESS: ML strategy helps Player 2 slightly\n")
} else {
  cat("○ NO IMPROVEMENT: ML strategy doesn't help (War may be too random)\n")
}

# Find best performing strategy
best_idx <- which.max(sapply(results, function(r) r$win_rate))
best_result <- results[[best_idx]]
cat("Best overall strategy:", best_result$name, "with", round(best_result$win_rate, 1), "% win rate\n")

cat("\n=== CONCLUSION ===\n")
cat("The ML approach learned a forfeit strategy from", length(training_examples), "examples.\n")
if (best_result$name == "ML Learned") {
  cat("🏆 The ML strategy is the best performer!\n")
} else {
  cat("🎯 The ML strategy ranked among the tested strategies.\n")
}
cat("Key insight: Optimal forfeit rate appears to be ~", round(results[[length(results)]]$forfeit_rate, 1), "%\n")
