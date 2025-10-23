# Enhanced War ML Strategy - Game State Analysis
cat("=== ENHANCED WAR ML: GAME STATE ANALYSIS ===\n\n")

# Simplified game functions
deck_simple <- function() {
  rep(2:14, 4)  # 2-10, J=11, Q=12, K=13, A=14
}

play_war_enhanced <- function(forfeit_strategy = NULL, collect_data = FALSE) {
  cards <- deck_simple()[sample(52)]
  p1 <- cards[1:26]
  p2 <- cards[27:52]
  
  rounds <- 0
  forfeits <- 0
  decision_data <- list()
  
  while (length(p1) > 0 && length(p2) > 0 && rounds < 500) {
    rounds <- rounds + 1
    
    c1 <- p1[1]
    c2 <- p2[1]
    p1 <- p1[-1]
    p2 <- p2[-1]
    
    # Enhanced game state features
    total_cards <- length(p1) + length(p2)
    state <- list(
      p2_cards = length(p2),
      p1_cards = length(p1),
      card_ratio = length(p2) / total_cards,
      current_card = c2,
      card_strength = c2 / 14,
      is_behind = length(p2) < length(p1),
      is_far_behind = length(p2) < (length(p1) * 0.7),
      p2_aces = sum(p2 == 14),
      p1_aces = sum(p1 == 14),
      p2_high_cards = sum(p2 >= 10),
      p1_high_cards = sum(p1 >= 10),
      p2_low_cards = sum(p2 <= 6),
      p1_low_cards = sum(p1 <= 6),
      game_progress = 1 - (total_cards / 52),
      ace_advantage = sum(p2 == 14) - sum(p1 == 14),
      high_advantage = sum(p2 >= 10) - sum(p1 >= 10)
    )
    
    # Forfeit decision
    forfeit <- FALSE
    if (!is.null(forfeit_strategy) && c1 != c2) {
      forfeit <- forfeit_strategy(state)
      if (forfeit) forfeits <- forfeits + 1
    }
    
    # Store decision data if collecting
    if (collect_data && c1 != c2) {
      immediate_outcome <- ifelse(forfeit, -1, ifelse(c2 > c1, 1, -1))
      decision_data[[length(decision_data) + 1]] <- list(
        state = state,
        forfeit = forfeit,
        immediate = immediate_outcome,
        won_round = !forfeit && c2 > c1
      )
    }
    
    # Apply decision
    if (forfeit) {
      p1 <- c(p1, c1, c2)
    } else if (c1 > c2) {
      p1 <- c(p1, c1, c2)
    } else if (c2 > c1) {
      p2 <- c(p2, c2, c1)
    } else {
      # Simple tie resolution
      p1 <- c(p1, c1)
      p2 <- c(p2, c2)
    }
  }
  
  winner <- ifelse(length(p1) > length(p2), 1, 2)
  return(list(
    winner = winner,
    p2_won = winner == 2,
    rounds = rounds,
    forfeits = forfeits,
    decisions = decision_data
  ))
}

# Generate comprehensive training data with diverse strategies
cat("Step 1: Generating comprehensive training data...\n")

all_decisions <- list()
n_games <- 2000

strategy_configs <- list(
  list(name = "Random Low", prob = 0.15, bias_behind = 1.2, bias_weak = 1.3),
  list(name = "Random Medium", prob = 0.25, bias_behind = 1.5, bias_weak = 1.4),
  list(name = "Random High", prob = 0.35, bias_behind = 1.8, bias_weak = 1.6),
  list(name = "Conservative", prob = 0.05, bias_behind = 2.0, bias_weak = 3.0),
  list(name = "Aggressive", prob = 0.40, bias_behind = 1.3, bias_weak = 1.2),
  list(name = "Situational", prob = 0.20, bias_behind = 2.5, bias_weak = 2.0)
)

for (game in 1:n_games) {
  if (game %% 400 == 0) cat("Game", game, "of", n_games, "\n")
  
  # Choose strategy config
  config <- sample(strategy_configs, 1)[[1]]
  
  # Create strategy based on config
  strategy <- function(state) {
    prob <- config$prob
    if (state$is_behind) prob <- prob * config$bias_behind
    if (state$card_strength < 0.4) prob <- prob * config$bias_weak
    if (state$game_progress > 0.7) prob <- prob * 1.2
    if (state$p2_aces == 0) prob <- prob * 1.3
    return(runif(1) < prob)
  }
  
  # Play game and collect decisions
  result <- play_war_enhanced(strategy, collect_data = TRUE)
  
  # Add game outcome to each decision
  for (decision in result$decisions) {
    decision$game_won <- result$p2_won
    decision$game_value <- ifelse(result$p2_won, 1, -1)
    decision$combined_value <- 0.3 * decision$immediate + 0.7 * decision$game_value
    all_decisions[[length(all_decisions) + 1]] <- decision
  }
}

cat("Collected", length(all_decisions), "decision examples\n")

# Step 2: Advanced game state analysis
cat("\nStep 2: Analyzing game states for optimal forfeit opportunities...\n")

# Create comprehensive game state categories
for (i in seq_along(all_decisions)) {
  state <- all_decisions[[i]]$state
  
  # Multi-dimensional game state classification
  card_position <- ifelse(state$card_ratio < 0.25, "VeryBehind",
                         ifelse(state$card_ratio < 0.4, "Behind", 
                               ifelse(state$card_ratio < 0.6, "Even", "Ahead")))
  
  card_quality <- ifelse(state$card_strength < 0.25, "VeryWeak",
                        ifelse(state$card_strength < 0.45, "Weak",
                              ifelse(state$card_strength < 0.65, "Medium", "Strong")))
  
  game_phase <- ifelse(state$game_progress < 0.3, "Early",
                      ifelse(state$game_progress < 0.7, "Mid", "Late"))
  
  ace_situation <- ifelse(state$p2_aces == 0, "NoAces",
                         ifelse(state$p2_aces == 1, "OneAce", "MultiAces"))
  
  advantage_type <- ifelse(state$ace_advantage <= -2, "AceDisadvantage",
                          ifelse(state$ace_advantage >= 2, "AceAdvantage", "AceNeutral"))
  
  all_decisions[[i]]$game_state <- paste(card_position, card_quality, game_phase, ace_situation, advantage_type, sep = "_")
}

# Analyze each game state
state_counts <- table(sapply(all_decisions, function(x) x$game_state))
frequent_states <- names(state_counts)[state_counts >= 30]  # Need enough samples

cat("Analyzing", length(frequent_states), "frequent game states...\n")

beneficial_analysis <- list()

for (state_name in frequent_states) {
  state_decisions <- all_decisions[sapply(all_decisions, function(x) x$game_state == state_name)]
  
  forfeit_decisions <- state_decisions[sapply(state_decisions, function(x) x$forfeit)]
  no_forfeit_decisions <- state_decisions[sapply(state_decisions, function(x) !x$forfeit)]
  
  if (length(forfeit_decisions) >= 10 && length(no_forfeit_decisions) >= 10) {
    # Compare outcomes
    forfeit_game_wins <- mean(sapply(forfeit_decisions, function(x) x$game_won))
    no_forfeit_game_wins <- mean(sapply(no_forfeit_decisions, function(x) x$game_won))
    
    forfeit_combined <- mean(sapply(forfeit_decisions, function(x) x$combined_value))
    no_forfeit_combined <- mean(sapply(no_forfeit_decisions, function(x) x$combined_value))
    
    # Calculate advantage
    win_rate_advantage <- forfeit_game_wins - no_forfeit_game_wins
    value_advantage <- forfeit_combined - no_forfeit_combined
    
    # Determine if this is a beneficial forfeit state
    is_beneficial <- (win_rate_advantage > 0.05 && value_advantage > 0.1)
    
    beneficial_analysis[[state_name]] <- list(
      n_total = length(state_decisions),
      n_forfeit = length(forfeit_decisions),
      n_no_forfeit = length(no_forfeit_decisions),
      forfeit_win_rate = forfeit_game_wins,
      no_forfeit_win_rate = no_forfeit_game_wins,
      forfeit_value = forfeit_combined,
      no_forfeit_value = no_forfeit_combined,
      win_advantage = win_rate_advantage,
      value_advantage = value_advantage,
      is_beneficial = is_beneficial
    )
  }
}

# Report findings
beneficial_states <- names(beneficial_analysis)[sapply(beneficial_analysis, function(x) x$is_beneficial)]

cat("\n=== GAME STATE ANALYSIS RESULTS ===\n")
if (length(beneficial_states) > 0) {
  cat("Found", length(beneficial_states), "game states where forfeiting is beneficial:\n\n")
  
  for (state in beneficial_states) {
    analysis <- beneficial_analysis[[state]]
    cat("State:", state, "\n")
    cat("  Sample size:", analysis$n_total, "(", analysis$n_forfeit, "forfeit,", analysis$n_no_forfeit, "no forfeit)\n")
    cat("  Forfeit win rate:", round(analysis$forfeit_win_rate * 100, 1), "% vs No-forfeit:", round(analysis$no_forfeit_win_rate * 100, 1), "%\n")
    cat("  Win rate advantage:", sprintf("%+.1f", analysis$win_advantage * 100), "percentage points\n")
    cat("  Value advantage:", sprintf("%+.3f", analysis$value_advantage), "\n\n")
  }
  
  # Create ML strategy based on beneficial states
  cat("Creating ML strategy based on discovered beneficial states...\n")
  
  ml_strategy_enhanced <- function(state) {
    # Recreate the state classification
    card_position <- ifelse(state$card_ratio < 0.25, "VeryBehind",
                           ifelse(state$card_ratio < 0.4, "Behind", 
                                 ifelse(state$card_ratio < 0.6, "Even", "Ahead")))
    
    card_quality <- ifelse(state$card_strength < 0.25, "VeryWeak",
                          ifelse(state$card_strength < 0.45, "Weak",
                                ifelse(state$card_strength < 0.65, "Medium", "Strong")))
    
    game_phase <- ifelse(state$game_progress < 0.3, "Early",
                        ifelse(state$game_progress < 0.7, "Mid", "Late"))
    
    ace_situation <- ifelse(state$p2_aces == 0, "NoAces",
                           ifelse(state$p2_aces == 1, "OneAce", "MultiAces"))
    
    advantage_type <- ifelse(state$ace_advantage <= -2, "AceDisadvantage",
                            ifelse(state$ace_advantage >= 2, "AceAdvantage", "AceNeutral"))
    
    current_state <- paste(card_position, card_quality, game_phase, ace_situation, advantage_type, sep = "_")
    
    return(current_state %in% beneficial_states)
  }
  
} else {
  cat("No clearly beneficial forfeit states discovered.\n")
  cat("This suggests that forfeiting is generally detrimental in War.\n\n")
  
  # Create conservative fallback strategy
  ml_strategy_enhanced <- function(state) {
    # Ultra-conservative: only forfeit in extremely desperate situations
    return(state$card_ratio < 0.15 && state$card_strength < 0.2 && state$game_progress > 0.8)
  }
}

# Step 3: Test the enhanced ML strategy
cat("Step 3: Testing enhanced ML strategy performance...\n")

strategies_to_test <- list(
  "No Forfeit" = NULL,
  "Always Forfeit 2-6" = function(s) s$current_card <= 6,
  "Behind + Weak" = function(s) s$is_behind && s$card_strength <= 0.4,
  "Conservative" = function(s) s$is_far_behind && s$card_strength <= 0.3 && s$p2_aces < 2,
  "Enhanced ML" = ml_strategy_enhanced
)

test_results <- list()

for (strategy_name in names(strategies_to_test)) {
  cat("Testing", strategy_name, "...\n")
  
  wins <- 0
  forfeits <- 0 
  rounds <- 0
  n_test <- 800
  
  set.seed(42)
  for (i in 1:n_test) {
    result <- play_war_enhanced(strategies_to_test[[strategy_name]])
    if (result$winner == 2) wins <- wins + 1
    forfeits <- forfeits + result$forfeits
    rounds <- rounds + result$rounds
  }
  
  win_rate <- wins / n_test * 100
  forfeit_rate <- ifelse(rounds > 0, forfeits / rounds * 100, 0)
  
  test_results[[strategy_name]] <- list(
    win_rate = win_rate,
    forfeit_rate = forfeit_rate
  )
  
  cat("  Win rate:", round(win_rate, 1), "% | Forfeit rate:", round(forfeit_rate, 1), "%\n")
}

# Final analysis
cat("\n=== FINAL ANALYSIS ===\n")
baseline_rate <- test_results[["No Forfeit"]]$win_rate
ml_rate <- test_results[["Enhanced ML"]]$win_rate
improvement <- ml_rate - baseline_rate

best_strategy <- names(test_results)[which.max(sapply(test_results, function(x) x$win_rate))]

cat("Baseline (No Forfeit):", round(baseline_rate, 1), "%\n")
cat("Enhanced ML Strategy:", round(ml_rate, 1), "%\n")
cat("Improvement:", sprintf("%+.1f", improvement), "percentage points\n")
cat("Best overall strategy:", best_strategy, "with", round(test_results[[best_strategy]]$win_rate, 1), "% win rate\n\n")

if (length(beneficial_states) > 0) {
  cat("✓ SUCCESS: ML discovered", length(beneficial_states), "beneficial forfeit situations!\n")
  cat("Key beneficial game states:\n")
  for (state in head(beneficial_states, 3)) {
    parts <- strsplit(state, "_")[[1]]
    cat("  -", parts[1], "position,", parts[2], "cards,", parts[3], "game,", parts[4], ",", parts[5], "\n")
  }
} else {
  cat("○ FINDING: No beneficial forfeit situations discovered\n")
  cat("This confirms that War is predominantly a game of chance where\n")
  cat("strategic forfeiting provides no significant advantage.\n")
}

if (improvement > 1) {
  cat("\n🏆 EXCELLENT: Enhanced ML strategy significantly improves performance!\n")
} else if (improvement > 0) {
  cat("\n✓ GOOD: Enhanced ML strategy provides modest improvement\n")
} else {
  cat("\n📊 INSIGHT: Analysis confirms optimal strategy is to play all cards\n")
}

cat("\n=== CONCLUSION ===\n")
cat("The enhanced ML analysis with comprehensive game state categorization\n")
cat("provides definitive insights into optimal War forfeit strategy.\n")
if (length(beneficial_states) > 0) {
  cat("Discovered forfeit opportunities in specific, rare game states.\n")
} else {
  cat("Confirmed that War's random nature makes forfeiting generally counterproductive.\n")
}
