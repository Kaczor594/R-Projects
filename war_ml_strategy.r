# Machine Learning Approach to Optimal War Forfeit Strategy
# Player 1: Always plays normally
# Player 2: Learns optimal forfeit strategy to maximize win probability

# Load required libraries
suppressMessages({
  if (!require(randomForest, quietly = TRUE)) {
    install.packages("randomForest", dependencies = TRUE, repos = "https://cloud.r-project.org/")
    library(randomForest)
  }
  if (!require(dplyr, quietly = TRUE)) {
    install.packages("dplyr", dependencies = TRUE, repos = "https://cloud.r-project.org/")
    library(dplyr)
  }
})

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

count_aces <- function(hand) {
  sum(hand$Value == "A")
}

count_high_cards <- function(hand) {
  # Count cards with rank >= 10 (10, J, Q, K, A)
  sum(hand$Rank >= 10)
}

count_low_cards <- function(hand) {
  # Count cards with rank <= 6 (2, 3, 4, 5, 6)
  sum(hand$Rank <= 6)
}

# Feature extraction function
extract_features <- function(player2_hand, player1_hand, player2_card) {
  total_cards <- nrow(player2_hand) + nrow(player1_hand)
  
  # Basic card counts
  p2_cards <- nrow(player2_hand)
  p1_cards <- nrow(player1_hand)
  
  # Card distribution features
  p2_aces <- count_aces(player2_hand)
  p2_high_cards <- count_high_cards(player2_hand)
  p2_low_cards <- count_low_cards(player2_hand)
  
  p1_aces <- count_aces(player1_hand)
  p1_high_cards <- count_high_cards(player1_hand)
  p1_low_cards <- count_low_cards(player1_hand)
  
  # Relative strength features
  card_advantage <- p2_cards - p1_cards
  card_ratio <- p2_cards / total_cards
  
  # Current card strength
  current_card_rank <- player2_card$Rank
  current_card_strength <- current_card_rank / 13  # Normalize to 0-1
  
  # Strategic features
  ace_advantage <- p2_aces - p1_aces
  high_card_advantage <- p2_high_cards - p1_high_cards
  
  # Position features (early, mid, late game)
  game_progress <- 1 - (total_cards / 52)  # 0 = start, 1 = end
  
  # Desperation indicators
  is_behind <- as.numeric(p2_cards < p1_cards)
  is_far_behind <- as.numeric(p2_cards < (p1_cards * 0.7))
  has_few_aces <- as.numeric(p2_aces < 2)
  
  return(data.frame(
    p2_cards = p2_cards,
    p1_cards = p1_cards,
    card_advantage = card_advantage,
    card_ratio = card_ratio,
    current_card_rank = current_card_rank,
    current_card_strength = current_card_strength,
    p2_aces = p2_aces,
    p1_aces = p1_aces,
    ace_advantage = ace_advantage,
    p2_high_cards = p2_high_cards,
    p1_high_cards = p1_high_cards,
    high_card_advantage = high_card_advantage,
    p2_low_cards = p2_low_cards,
    p1_low_cards = p1_low_cards,
    game_progress = game_progress,
    is_behind = is_behind,
    is_far_behind = is_far_behind,
    has_few_aces = has_few_aces
  ))
}

# Simulate a game with a given forfeit strategy function
simulate_game_with_strategy <- function(forfeit_function = NULL) {
  d <- deck()
  shuffled <- shuffle_deck(d)
  hands <- deal_cards(shuffled)
  player1 <- hands$player1
  player2 <- hands$player2
  
  rounds <- 0
  forfeits <- 0
  game_data <- list()
  
  while (nrow(player1) > 0 && nrow(player2) > 0 && rounds < 2000) {
    rounds <- rounds + 1
    
    # Get top cards
    card1 <- player1[1, ]
    card2 <- player2[1, ]
    
    # Remove cards from hands
    player1 <- player1[-1, ]
    player2 <- player2[-1, ]
    
    # Extract features for this decision point
    features <- extract_features(player2, player1, card2)
    
    # Determine if Player 2 should forfeit (skip ties for forfeit decisions)
    should_forfeit <- FALSE
    if (!is.null(forfeit_function) && card1$Rank != card2$Rank) {
      should_forfeit <- forfeit_function(features)
      if (should_forfeit) forfeits <- forfeits + 1
    }
    
    # Store decision data
    outcome <- NA
    if (should_forfeit) {
      outcome <- "forfeit"
      player1 <- rbind(player1, card1, card2)
    } else if (card1$Rank > card2$Rank) {
      outcome <- "lose"
      player1 <- rbind(player1, card1, card2)
    } else if (card1$Rank < card2$Rank) {
      outcome <- "win"
      player2 <- rbind(player2, card2, card1)
    } else {
      # War - simplified handling
      outcome <- "war"
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
          # Split on repeated tie
          player1 <- rbind(player1, card1, war_cards1)
          player2 <- rbind(player2, card2, war_cards2)
        }
      } else {
        break
      }
    }
    
    # Store this round's data
    game_data[[rounds]] <- list(
      features = features,
      decision = should_forfeit,
      outcome = outcome,
      round = rounds
    )
  }
  
  # Determine winner
  winner <- ifelse(nrow(player1) > nrow(player2), 1, 
                  ifelse(nrow(player2) > nrow(player1), 2, 0))
  
  return(list(
    winner = winner,
    rounds = rounds,
    forfeits = forfeits,
    p2_won = winner == 2,
    game_data = game_data
  ))
}

# Generate training data using diverse forfeit strategies
generate_training_data <- function(n_games = 1000) {
  cat("Generating training data from", n_games, "games with diverse strategies...\n")
  
  all_data <- list()
  data_count <- 0
  
  # Define different types of forfeit strategies to explore
  strategy_types <- c("random", "conservative", "aggressive", "situational", "desperate", "none")
  
  for (game_num in 1:n_games) {
    if (game_num %% 100 == 0) {
      cat("Game", game_num, "of", n_games, "\n")
    }
    
    # Choose a strategy type for this game
    strategy_type <- sample(strategy_types, 1)
    
    # Create diverse forfeit strategies to explore different game states
    random_forfeit <- switch(strategy_type,
      "random" = {
        forfeit_prob <- runif(1, 0, 0.4)
        function(features) {
          base_prob <- forfeit_prob
          if (features$is_behind) base_prob <- base_prob * 1.3
          if (features$has_few_aces) base_prob <- base_prob * 1.2
          if (features$current_card_strength < 0.4) base_prob <- base_prob * 1.4
          return(runif(1) < base_prob)
        }
      },
      "conservative" = {
        function(features) {
          return(features$is_far_behind && features$current_card_strength < 0.3 && 
                 features$game_progress > 0.5 && features$has_few_aces)
        }
      },
      "aggressive" = {
        function(features) {
          return(features$current_card_strength < 0.5 && 
                 (features$is_behind || features$has_few_aces || features$game_progress > 0.7))
        }
      },
      "situational" = {
        function(features) {
          # Complex situational strategy
          weak_late <- features$current_card_strength < 0.4 && features$game_progress > 0.6
          desperate <- features$card_ratio < 0.3 && features$current_card_strength < 0.6
          ace_poor <- features$p2_aces == 0 && features$current_card_strength < 0.5
          return(weak_late || desperate || ace_poor)
        }
      },
      "desperate" = {
        function(features) {
          # Only forfeit in very desperate situations
          return(features$card_ratio < 0.2 || 
                 (features$is_far_behind && features$current_card_strength < 0.2))
        }
      },
      "none" = {
        function(features) FALSE  # Never forfeit
      }
    )
    
    # Simulate game
    game_result <- simulate_game_with_strategy(random_forfeit)
    
    # Extract data from each decision point
    for (round_data in game_result$game_data) {
      if (round_data$outcome %in% c("win", "lose", "forfeit")) {
        data_count <- data_count + 1
        
        # Calculate immediate outcome value
        immediate_value <- switch(round_data$outcome,
          "win" = 1,
          "lose" = -1,
          "forfeit" = -1  # Forfeiting is like losing the round
        )
        
        # Calculate long-term value based on game outcome
        game_value <- ifelse(game_result$p2_won, 1, -1)
        
        # Combine immediate and long-term value
        combined_value <- 0.3 * immediate_value + 0.7 * game_value
        
        all_data[[data_count]] <- list(
          features = round_data$features,
          forfeited = round_data$decision,
          immediate_value = immediate_value,
          game_value = game_value,
          combined_value = combined_value,
          round = round_data$round,
          game = game_num
        )
      }
    }
  }
  
  # Convert to data frame
  feature_df <- do.call(rbind, lapply(all_data, function(x) x$features))
  
  training_data <- data.frame(
    feature_df,
    forfeited = sapply(all_data, function(x) x$forfeited),
    immediate_value = sapply(all_data, function(x) x$immediate_value),
    game_value = sapply(all_data, function(x) x$game_value),
    combined_value = sapply(all_data, function(x) x$combined_value),
    round = sapply(all_data, function(x) x$round),
    game = sapply(all_data, function(x) x$game)
  )
  
  cat("Generated", nrow(training_data), "training examples from", n_games, "games\n")
  return(training_data)
}

# Train ML model to predict optimal forfeit decisions
train_forfeit_model <- function(training_data) {
  cat("Training Random Forest model with game state analysis...\n")
  
  # Prepare features
  feature_cols <- c("p2_cards", "p1_cards", "card_advantage", "card_ratio", 
                   "current_card_rank", "current_card_strength", "p2_aces", "p1_aces", 
                   "ace_advantage", "p2_high_cards", "p1_high_cards", "high_card_advantage",
                   "p2_low_cards", "p1_low_cards", "game_progress", "is_behind", 
                   "is_far_behind", "has_few_aces")
  
  # Advanced game state analysis - find situations where forfeiting actually helps
  cat("Analyzing game states where forfeiting might be beneficial...\n")
  
  # Create discretized game states for analysis
  training_data$game_state <- paste0(
    ifelse(training_data$card_ratio < 0.3, "VeryBehind", 
           ifelse(training_data$card_ratio < 0.4, "Behind", 
                  ifelse(training_data$card_ratio < 0.6, "Even", "Ahead"))),
    "_",
    ifelse(training_data$current_card_strength < 0.3, "VeryWeak",
           ifelse(training_data$current_card_strength < 0.5, "Weak",
                  ifelse(training_data$current_card_strength < 0.7, "Medium", "Strong"))),
    "_",
    ifelse(training_data$game_progress < 0.3, "Early",
           ifelse(training_data$game_progress < 0.7, "Mid", "Late")),
    "_",
    ifelse(training_data$p2_aces == 0, "NoAces",
           ifelse(training_data$p2_aces == 1, "OneAce", "MultiAces"))
  )
  
  # Analyze each game state combination
  state_analysis <- list()
  unique_states <- unique(training_data$game_state)
  
  for (state in unique_states) {
    state_data <- training_data[training_data$game_state == state, ]
    
    if (nrow(state_data) >= 20) {  # Need enough samples
      forfeit_data <- state_data[state_data$forfeited == TRUE, ]
      no_forfeit_data <- state_data[state_data$forfeited == FALSE, ]
      
      if (nrow(forfeit_data) >= 5 && nrow(no_forfeit_data) >= 5) {
        # Compare outcomes when forfeiting vs not forfeiting in this state
        forfeit_win_rate <- mean(forfeit_data$game_value > 0)
        no_forfeit_win_rate <- mean(no_forfeit_data$game_value > 0)
        
        forfeit_avg_value <- mean(forfeit_data$combined_value)
        no_forfeit_avg_value <- mean(no_forfeit_data$combined_value)
        
        # Calculate if forfeiting is beneficial in this state
        forfeit_advantage <- forfeit_avg_value - no_forfeit_avg_value
        win_rate_diff <- forfeit_win_rate - no_forfeit_win_rate
        
        state_analysis[[state]] <- list(
          n_samples = nrow(state_data),
          n_forfeit = nrow(forfeit_data),
          n_no_forfeit = nrow(no_forfeit_data),
          forfeit_win_rate = forfeit_win_rate,
          no_forfeit_win_rate = no_forfeit_win_rate,
          forfeit_avg_value = forfeit_avg_value,
          no_forfeit_avg_value = no_forfeit_avg_value,
          forfeit_advantage = forfeit_advantage,
          win_rate_diff = win_rate_diff,
          should_forfeit = forfeit_advantage > 0.1 && win_rate_diff > 0.05  # Conservative threshold
        )
      }
    }
  }
  
  # Find beneficial forfeit states
  beneficial_states <- names(state_analysis)[sapply(state_analysis, function(x) x$should_forfeit)]
  
  cat("Found", length(beneficial_states), "game states where forfeiting may be beneficial:\n")
  for (state in beneficial_states) {
    analysis <- state_analysis[[state]]
    cat("  ", state, ": Advantage =", round(analysis$forfeit_advantage, 3), 
        ", Win rate diff =", round(analysis$win_rate_diff, 3), "\n")
  }
  
  # Create refined target variable based on discovered beneficial states
  training_data$should_forfeit_optimal <- training_data$game_state %in% beneficial_states
  
  # Additional condition-based refinement for edge cases
  # Look for patterns in the most beneficial forfeit situations
  if (length(beneficial_states) > 0) {
    # Analyze the characteristics of beneficial states
    beneficial_data <- training_data[training_data$game_state %in% beneficial_states, ]
    
    # Extract common patterns
    avg_card_ratio <- mean(beneficial_data$card_ratio)
    avg_card_strength <- mean(beneficial_data$current_card_strength)
    avg_game_progress <- mean(beneficial_data$game_progress)
    common_behind <- mean(beneficial_data$is_behind) > 0.5
    common_few_aces <- mean(beneficial_data$has_few_aces) > 0.5
    
    cat("Beneficial forfeit patterns:\n")
    cat("  Average card ratio:", round(avg_card_ratio, 3), "\n")
    cat("  Average card strength:", round(avg_card_strength, 3), "\n") 
    cat("  Average game progress:", round(avg_game_progress, 3), "\n")
    cat("  Commonly behind:", common_behind, "\n")
    cat("  Commonly few aces:", common_few_aces, "\n")
    
    # Add pattern-based forfeit conditions for similar situations
    pattern_forfeit <- (
      (training_data$card_ratio <= (avg_card_ratio + 0.05)) &
      (training_data$current_card_strength <= (avg_card_strength + 0.1)) &
      (training_data$game_progress >= (avg_game_progress - 0.2)) &
      (if (common_behind) training_data$is_behind else TRUE) &
      (if (common_few_aces) training_data$has_few_aces else TRUE)
    )
    
    # Combine with state-based forfeits
    training_data$should_forfeit_optimal <- training_data$should_forfeit_optimal | pattern_forfeit
  } else {
    cat("No clearly beneficial forfeit states found. Using conservative heuristics.\n")
    
    # Fallback to very conservative forfeit conditions
    training_data$should_forfeit_optimal <- (
      (training_data$card_ratio < 0.25) &  # Very far behind
      (training_data$current_card_strength < 0.25) &  # Very weak cards (2-3)
      (training_data$game_progress > 0.6) &  # Late game
      (training_data$has_few_aces == 1)  # Few aces
    )
  }
  
  # Final analysis of the training target
  forfeit_rate <- mean(training_data$should_forfeit_optimal)
  cat("Final optimal forfeit rate in training data:", round(forfeit_rate * 100, 2), "%\n")
  
  if (forfeit_rate > 0) {
    optimal_forfeit_data <- training_data[training_data$should_forfeit_optimal, ]
    cat("Characteristics of optimal forfeit situations:\n")
    cat("  Avg card ratio:", round(mean(optimal_forfeit_data$card_ratio), 3), "\n")
    cat("  Avg card strength:", round(mean(optimal_forfeit_data$current_card_strength), 3), "\n")
    cat("  % behind:", round(mean(optimal_forfeit_data$is_behind) * 100, 1), "%\n")
    cat("  % with few aces:", round(mean(optimal_forfeit_data$has_few_aces) * 100, 1), "%\n")
  }
  
  # Train model
  model <- randomForest(
    x = training_data[feature_cols],
    y = as.factor(training_data$should_forfeit_optimal),
    ntree = 100,
    mtry = sqrt(length(feature_cols)),
    importance = TRUE
  )
  
  cat("Model training complete!\n")
  cat("Feature importance:\n")
  print(importance(model))
  
  return(list(model = model, feature_cols = feature_cols))
}

# Create ML-based forfeit strategy
create_ml_forfeit_strategy <- function(trained_model) {
  model <- trained_model$model
  feature_cols <- trained_model$feature_cols
  
  return(function(features) {
    prediction <- predict(model, features[feature_cols])
    return(as.character(prediction) == "TRUE")
  })
}

# Test different strategies
test_strategies <- function(n_test_games = 500) {
  cat("\n=== TESTING DIFFERENT FORFEIT STRATEGIES ===\n")
  
  strategies <- list(
    "No Forfeit" = NULL,
    "Always Forfeit Low Cards" = function(f) f$current_card_rank <= 6,
    "Conservative" = function(f) f$is_behind && f$current_card_rank <= 7 && f$has_few_aces,
    "Aggressive" = function(f) f$current_card_rank <= 8 && (f$is_behind || f$has_few_aces)
  )
  
  results <- list()
  
  for (strategy_name in names(strategies)) {
    cat("Testing", strategy_name, "strategy...\n")
    
    wins <- 0
    total_forfeits <- 0
    total_rounds <- 0
    
    set.seed(42)  # For reproducible results
    for (i in 1:n_test_games) {
      game_result <- simulate_game_with_strategy(strategies[[strategy_name]])
      if (game_result$winner == 2) wins <- wins + 1
      total_forfeits <- total_forfeits + game_result$forfeits
      total_rounds <- total_rounds + game_result$rounds
    }
    
    win_rate <- wins / n_test_games * 100
    forfeit_rate <- ifelse(total_rounds > 0, total_forfeits / total_rounds * 100, 0)
    
    results[[strategy_name]] <- list(
      win_rate = win_rate,
      wins = wins,
      games = n_test_games,
      forfeit_rate = forfeit_rate,
      total_forfeits = total_forfeits,
      total_rounds = total_rounds
    )
    
    cat("  Win rate:", round(win_rate, 2), "%\n")
    cat("  Forfeit rate:", round(forfeit_rate, 2), "%\n\n")
  }
  
  return(results)
}

# Main execution
cat("=== WAR GAME ML STRATEGY OPTIMIZATION ===\n")
cat("==========================================\n\n")

# Step 1: Generate training data
cat("Step 1: Generating training data...\n")
training_data <- generate_training_data(5000)  # More data for better game state analysis

# Step 2: Train ML model
cat("\nStep 2: Training ML model...\n")
trained_model <- train_forfeit_model(training_data)

# Step 3: Create ML strategy
cat("\nStep 3: Creating ML-based strategy...\n")
ml_strategy <- create_ml_forfeit_strategy(trained_model)

# Step 4: Test baseline strategies
cat("\nStep 4: Testing baseline strategies...\n")
baseline_results <- test_strategies(1000)

# Step 5: Test ML strategy
cat("Step 5: Testing ML-optimized strategy...\n")
ml_wins <- 0
ml_forfeits <- 0
ml_rounds <- 0
n_ml_games <- 1000

set.seed(42)
for (i in 1:n_ml_games) {
  if (i %% 100 == 0) cat("ML test game", i, "of", n_ml_games, "\n")
  
  game_result <- simulate_game_with_strategy(ml_strategy)
  if (game_result$winner == 2) ml_wins <- ml_wins + 1
  ml_forfeits <- ml_forfeits + game_result$forfeits
  ml_rounds <- ml_rounds + game_result$rounds
}

ml_win_rate <- ml_wins / n_ml_games * 100
ml_forfeit_rate <- ifelse(ml_rounds > 0, ml_forfeits / ml_rounds * 100, 0)

# Step 6: Compare results
cat("\n=== STRATEGY COMPARISON RESULTS ===\n")
cat("====================================\n")

all_results <- baseline_results
all_results[["ML Optimized"]] <- list(
  win_rate = ml_win_rate,
  wins = ml_wins,
  games = n_ml_games,
  forfeit_rate = ml_forfeit_rate,
  total_forfeits = ml_forfeits,
  total_rounds = ml_rounds
)

# Find best strategy
best_strategy <- names(all_results)[which.max(sapply(all_results, function(x) x$win_rate))]

for (name in names(all_results)) {
  result <- all_results[[name]]
  marker <- ifelse(name == best_strategy, " *** BEST ***", "")
  
  cat(sprintf("%-20s: %5.1f%% win rate (%d/%d games), %5.1f%% forfeit rate%s\n", 
              name, 
              result$win_rate, 
              result$wins, 
              result$games,
              result$forfeit_rate,
              marker))
}

cat("\n=== ANALYSIS ===\n")
no_forfeit_rate <- all_results[["No Forfeit"]]$win_rate
ml_improvement <- ml_win_rate - no_forfeit_rate

cat("Baseline (No Forfeit) win rate:", round(no_forfeit_rate, 2), "%\n")
cat("ML Strategy win rate:", round(ml_win_rate, 2), "%\n")
cat("Improvement from ML strategy:", sprintf("%+.2f", ml_improvement), "percentage points\n")

if (ml_improvement > 1) {
  cat("✓ ML STRATEGY SIGNIFICANTLY IMPROVES Player 2's performance!\n")
} else if (ml_improvement > 0) {
  cat("✓ ML STRATEGY SLIGHTLY IMPROVES Player 2's performance\n")
} else {
  cat("○ ML STRATEGY shows no significant improvement\n")
}

cat("\nMost important features for forfeit decisions:\n")
feature_importance <- importance(trained_model$model)
top_features <- head(feature_importance[order(feature_importance[,1], decreasing = TRUE), , drop = FALSE], 8)
print(top_features)

# Test the ML strategy on specific game states to see when it forfeits
cat("\n=== GAME STATE ANALYSIS ===\n")
test_ml_on_states <- function(ml_strategy) {
  cat("Testing ML strategy behavior on different game states:\n")
  
  # Create test scenarios
  test_scenarios <- list(
    "Very Behind + Very Weak" = list(p2_cards = 5, p1_cards = 20, current_card_rank = 3, p2_aces = 0),
    "Behind + Weak + Late Game" = list(p2_cards = 8, p1_cards = 15, current_card_rank = 5, p2_aces = 1),
    "Even + Medium + Early Game" = list(p2_cards = 13, p1_cards = 13, current_card_rank = 7, p2_aces = 2),
    "Ahead + Strong" = list(p2_cards = 18, p1_cards = 8, current_card_rank = 12, p2_aces = 2),
    "Desperate + Any Card" = list(p2_cards = 3, p1_cards = 22, current_card_rank = 9, p2_aces = 0),
    "Close + Weak + No Aces" = list(p2_cards = 12, p1_cards = 14, current_card_rank = 4, p2_aces = 0)
  )
  
  for (scenario_name in names(test_scenarios)) {
    scenario <- test_scenarios[[scenario_name]]
    
    # Create features for this scenario
    total_cards <- scenario$p2_cards + scenario$p1_cards
    features <- data.frame(
      p2_cards = scenario$p2_cards,
      p1_cards = scenario$p1_cards,
      card_advantage = scenario$p2_cards - scenario$p1_cards,
      card_ratio = scenario$p2_cards / total_cards,
      current_card_rank = scenario$current_card_rank,
      current_card_strength = scenario$current_card_rank / 13,
      p2_aces = scenario$p2_aces,
      p1_aces = 1,  # Assume opponent has 1 ace
      ace_advantage = scenario$p2_aces - 1,
      p2_high_cards = max(0, scenario$p2_cards * 0.3),  # Estimate
      p1_high_cards = max(0, scenario$p1_cards * 0.3),  # Estimate
      high_card_advantage = max(0, scenario$p2_cards * 0.3) - max(0, scenario$p1_cards * 0.3),
      p2_low_cards = max(0, scenario$p2_cards * 0.4),  # Estimate
      p1_low_cards = max(0, scenario$p1_cards * 0.4),  # Estimate
      game_progress = 1 - (total_cards / 52),
      is_behind = as.numeric(scenario$p2_cards < scenario$p1_cards),
      is_far_behind = as.numeric(scenario$p2_cards < (scenario$p1_cards * 0.7)),
      has_few_aces = as.numeric(scenario$p2_aces < 2)
    )
    
    decision <- ml_strategy(features)
    card_name <- switch(as.character(scenario$current_card_rank),
                       "11" = "J", "12" = "Q", "13" = "K", "14" = "A", 
                       as.character(scenario$current_card_rank))
    
    cat("  ", sprintf("%-25s", scenario_name), ": ", 
        sprintf("P2:%2d P1:%2d Card:%2s", scenario$p2_cards, scenario$p1_cards, card_name),
        " -> ", ifelse(decision, "FORFEIT", "PLAY"), "\n")
  }
}

test_ml_on_states(ml_strategy)

cat("\n=== CONCLUSION ===\n")
cat("The ML model learned to optimize forfeit decisions based on specific game states.\n")
cat("Key insights:\n")
cat("- Forfeit frequency:", round(ml_forfeit_rate, 1), "% of rounds\n")
cat("- Performance vs baseline:", sprintf("%+.1f", ml_improvement), " percentage points\n")

if (ml_improvement > 0.5) {
  cat("- SUCCESS: ML found beneficial forfeit situations!\n")
} else if (ml_improvement > -0.5) {
  cat("- NEUTRAL: ML forfeit strategy shows minimal impact\n") 
} else {
  cat("- CHALLENGE: ML strategy may need more refinement\n")
}

if (best_strategy == "ML Optimized") {
  cat("- The ML strategy is the BEST performing strategy!\n")
} else {
  cat("- Best strategy was:", best_strategy, "\n")
  cat("- ML strategy can be further improved by analyzing why", best_strategy, "performed better\n")
}
