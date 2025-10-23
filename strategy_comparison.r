# Streamlined strategy comparison for War card game
# Source: Compare forfeit strategy vs normal play for Player 2

# Basic deck and game functions
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

should_forfeit <- function(player_hand, opponent_hand, player_card_rank) {
  total_cards <- nrow(player_hand) + nrow(opponent_hand)
  player_cards <- nrow(player_hand)
  player_aces <- count_aces(player_hand)
  
  has_fewer_than_half <- player_cards < (total_cards / 2)
  has_fewer_than_2_aces <- player_aces < 2
  plays_card_lower_than_9 <- player_card_rank < 9
  
  return(has_fewer_than_half && has_fewer_than_2_aces && plays_card_lower_than_9)
}

# Simplified game simulation
simulate_game_with_forfeit <- function() {
  d <- deck()
  shuffled <- shuffle_deck(d)
  hands <- deal_cards(shuffled)
  player1 <- hands$player1
  player2 <- hands$player2
  
  rounds <- 0
  forfeits <- 0
  
  while (nrow(player1) > 0 && nrow(player2) > 0 && rounds < 1000) {
    rounds <- rounds + 1
    
    card1 <- player1[1, ]
    card2 <- player2[1, ]
    player1 <- player1[-1, ]
    player2 <- player2[-1, ]
    
    # Check forfeit strategy for Player 2
    if (card1$Rank != card2$Rank && should_forfeit(player2, player1, card2$Rank)) {
      forfeits <- forfeits + 1
      player1 <- rbind(player1, card1, card2)
    } else if (card1$Rank > card2$Rank) {
      player1 <- rbind(player1, card1, card2)
    } else if (card1$Rank < card2$Rank) {
      player2 <- rbind(player2, card2, card1)
    } else {
      # War - simplified version
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
          # Multiple ties - split cards
          player1 <- rbind(player1, card1, war_cards1)
          player2 <- rbind(player2, card2, war_cards2)
        }
      } else {
        break
      }
    }
  }
  
  winner <- ifelse(nrow(player1) > nrow(player2), 1, 
                  ifelse(nrow(player2) > nrow(player1), 2, 0))
  return(list(winner = winner, rounds = rounds, forfeits = forfeits))
}

# Simplified game without forfeit
simulate_game_no_forfeit <- function() {
  d <- deck()
  shuffled <- shuffle_deck(d)
  hands <- deal_cards(shuffled)
  player1 <- hands$player1
  player2 <- hands$player2
  
  rounds <- 0
  
  while (nrow(player1) > 0 && nrow(player2) > 0 && rounds < 1000) {
    rounds <- rounds + 1
    
    card1 <- player1[1, ]
    card2 <- player2[1, ]
    player1 <- player1[-1, ]
    player2 <- player2[-1, ]
    
    if (card1$Rank > card2$Rank) {
      player1 <- rbind(player1, card1, card2)
    } else if (card1$Rank < card2$Rank) {
      player2 <- rbind(player2, card2, card1)
    } else {
      # War - simplified version
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
          # Multiple ties - split cards
          player1 <- rbind(player1, card1, war_cards1)
          player2 <- rbind(player2, card2, war_cards2)
        }
      } else {
        break
      }
    }
  }
  
  winner <- ifelse(nrow(player1) > nrow(player2), 1, 
                  ifelse(nrow(player2) > nrow(player1), 2, 0))
  return(list(winner = winner, rounds = rounds))
}

# Run comparison
cat("FORFEIT STRATEGY EFFECTIVENESS TEST\n")
cat("====================================\n")

num_games <- 1000
cat("Testing", num_games, "games for each strategy...\n\n")

# Test with forfeit strategy
cat("Running games WITH forfeit strategy...\n")
forfeit_wins <- 0
forfeit_total_rounds <- 0
forfeit_total_forfeits <- 0

set.seed(123)  # For reproducible results
for (i in 1:num_games) {
  result <- simulate_game_with_forfeit()
  if (result$winner == 2) forfeit_wins <- forfeit_wins + 1
  forfeit_total_rounds <- forfeit_total_rounds + result$rounds
  forfeit_total_forfeits <- forfeit_total_forfeits + result$forfeits
}

forfeit_win_rate <- forfeit_wins / num_games * 100

# Test without forfeit strategy  
cat("Running games WITHOUT forfeit strategy (baseline)...\n")
baseline_wins <- 0
baseline_total_rounds <- 0

set.seed(123)  # Same seed for fair comparison
for (i in 1:num_games) {
  result <- simulate_game_no_forfeit()
  if (result$winner == 2) baseline_wins <- baseline_wins + 1
  baseline_total_rounds <- baseline_total_rounds + result$rounds
}

baseline_win_rate <- baseline_wins / num_games * 100

# Results
cat("\n")
cat("RESULTS COMPARISON\n")
cat("==================\n")
cat("Baseline (No Forfeit):\n")
cat("  Player 2 wins:", baseline_wins, "out of", num_games, "games\n")
cat("  Win rate:", round(baseline_win_rate, 2), "%\n")
cat("  Average rounds per game:", round(baseline_total_rounds/num_games, 1), "\n")

cat("\nWith Forfeit Strategy:\n")
cat("  Player 2 wins:", forfeit_wins, "out of", num_games, "games\n") 
cat("  Win rate:", round(forfeit_win_rate, 2), "%\n")
cat("  Average rounds per game:", round(forfeit_total_rounds/num_games, 1), "\n")
cat("  Total forfeits:", forfeit_total_forfeits, "\n")
cat("  Forfeit rate:", round(forfeit_total_forfeits/forfeit_total_rounds*100, 2), "% of rounds\n")

# Analysis
difference <- forfeit_win_rate - baseline_win_rate
cat("\n")
cat("STRATEGY IMPACT\n")
cat("===============\n")
cat("Win rate difference:", sprintf("%+.2f", difference), "percentage points\n")

if (difference > 1) {
  cat("✓ FORFEIT STRATEGY HELPS Player 2\n")
  cat("  Player 2 wins MORE often with forfeit strategy\n")
} else if (difference < -1) {
  cat("✗ FORFEIT STRATEGY HURTS Player 2\n") 
  cat("  Player 2 wins LESS often with forfeit strategy\n")
} else {
  cat("○ FORFEIT STRATEGY has minimal impact\n")
  cat("  No significant change in Player 2's win rate\n")
}

cat("\nConclusion: The forfeit strategy appears to")
if (difference > 0) {
  cat(" IMPROVE")
} else if (difference < 0) {
  cat(" WORSEN")
} else {
  cat(" have NO significant effect on")
}
cat(" Player 2's chances of winning.\n")
