# Card Game War with Forfeit Strategy - Demo Version
# A player forfeits if they have: < 50% cards AND < 2 Aces AND play card < 9

# Create deck function
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

# Helper function to count aces in a player's hand
count_aces <- function(hand) {
  sum(hand$Value == "A")
}

# Helper function to check if a player should forfeit based on strategy rules
should_forfeit <- function(player_hand, opponent_hand, player_card_rank) {
  total_cards <- nrow(player_hand) + nrow(opponent_hand)
  player_cards <- nrow(player_hand)
  player_aces <- count_aces(player_hand)
  
  # Check if player has fewer than half the cards AND has less than 2 aces
  has_fewer_than_half <- player_cards < (total_cards / 2)
  has_fewer_than_2_aces <- player_aces < 2
  plays_card_lower_than_9 <- player_card_rank < 9  # Rank 9 corresponds to card value 9
  
  return(has_fewer_than_half && has_fewer_than_2_aces && plays_card_lower_than_9)
}

# Simple game simulation with forfeit strategy
simulate_game_with_strategy <- function(verbose = TRUE) {
  d <- deck()
  shuffled <- shuffle_deck(d)
  hands <- deal_cards(shuffled)
  player1 <- hands$player1
  player2 <- hands$player2
  
  round_num <- 1
  forfeits_p1 <- 0
  forfeits_p2 <- 0
  
  while (nrow(player1) > 0 && nrow(player2) > 0) {
    # Get the top cards
    card1 <- player1[1, ]
    card2 <- player2[1, ]
    
    # Remove the top cards from each hand
    player1 <- player1[-1, ]
    player2 <- player2[-1, ]
    
    # Only Player 2 uses forfeit strategy for non-tie rounds
    player2_should_forfeit <- should_forfeit(player2, player1, card2$Rank)
    
    # Determine winner and redistribute cards
    if (player2_should_forfeit && card1$Rank != card2$Rank) {
      # Player 2 forfeits only if it's not a tie
      if (verbose) cat("Round", round_num, ": Player 2 forfeits (", card2$Value, "vs", card1$Value, ") - Player 1 wins\n")
      player1 <- rbind(player1, card1, card2)
      forfeits_p2 <- forfeits_p2 + 1
    } else if (card1$Rank > card2$Rank) {
      if (verbose) cat("Round", round_num, ": Player 1 wins (", card1$Value, "vs", card2$Value, ")\n")
      player1 <- rbind(player1, card1, card2)
    } else if (card1$Rank < card2$Rank) {
      if (verbose) cat("Round", round_num, ": Player 2 wins (", card2$Value, "vs", card1$Value, ")\n")
      player2 <- rbind(player2, card2, card1)
    } else {
      # It's a tie - WAR! (no forfeit strategy applies during wars)
      if (verbose) cat("Round", round_num, ": War!\n")
      # Simple war resolution for demo
      player1 <- rbind(player1, card1)
      player2 <- rbind(player2, card2)
    }
    
    round_num <- round_num + 1
    
    # Safety limit
    if (round_num > 1000) {
      if (verbose) cat("Game ended after 1000 rounds\n")
      break
    }
  }
  
  # Game results
  if (nrow(player1) > nrow(player2)) {
    winner <- "Player 1"
  } else if (nrow(player2) > nrow(player1)) {
    winner <- "Player 2"
  } else {
    winner <- "Tie"
  }
  
  if (verbose) {
    cat("GAME OVER:", winner, "wins!\n")
    cat("Final cards - Player 1:", nrow(player1), ", Player 2:", nrow(player2), "\n")
    cat("Forfeits - Player 1:", forfeits_p1, ", Player 2:", forfeits_p2, "\n\n")
  }
  
  return(list(
    winner = winner,
    rounds = round_num - 1,
    forfeits_p1 = forfeits_p1,
    forfeits_p2 = forfeits_p2,
    final_cards_p1 = nrow(player1),
    final_cards_p2 = nrow(player2)
  ))
}

# Demo the forfeit strategy
cat("=== WAR CARD GAME WITH FORFEIT STRATEGY ===\n")
cat("Strategy Rules:\n")
cat("- ONLY Player 2 forfeits if they have < 26 cards AND < 2 Aces AND play card < 9\n")
cat("- Forfeit strategy does NOT apply during wars - all wars played normally\n")
cat("- Cards that trigger forfeit: 2, 3, 4, 5, 6, 7, 8\n")
cat("- Cards that don't trigger forfeit: 9, 10, J, Q, K, A\n")
cat("- Player 1 NEVER forfeits - always plays normally\n\n")

# Run a single demo game
cat("=== DEMO GAME ===\n")
demo_result <- simulate_game_with_strategy(verbose = TRUE)

# Run multiple games to see strategy impact
cat("=== RUNNING 100 GAMES TO ANALYZE STRATEGY ===\n")
results <- list()
for (i in 1:100) {
  results[[i]] <- simulate_game_with_strategy(verbose = FALSE)
}

# Analyze results
p1_wins <- sum(sapply(results, function(x) x$winner == "Player 1"))
p2_wins <- sum(sapply(results, function(x) x$winner == "Player 2"))
ties <- sum(sapply(results, function(x) x$winner == "Tie"))

total_forfeits_p1 <- sum(sapply(results, function(x) x$forfeits_p1))
total_forfeits_p2 <- sum(sapply(results, function(x) x$forfeits_p2))
total_rounds <- sum(sapply(results, function(x) x$rounds))

cat("\n=== STRATEGY ANALYSIS (100 games) ===\n")
cat("Player 1 wins:", p1_wins, "(", round(p1_wins/100*100, 1), "%)\n")
cat("Player 2 wins:", p2_wins, "(", round(p2_wins/100*100, 1), "%)\n")
cat("Ties:", ties, "(", round(ties/100*100, 1), "%)\n")
cat("Average rounds per game:", round(total_rounds/100, 1), "\n")
cat("Total forfeits by Player 1:", total_forfeits_p1, "\n")
cat("Total forfeits by Player 2:", total_forfeits_p2, "\n")
cat("Forfeit rate:", round((total_forfeits_p1 + total_forfeits_p2)/total_rounds*100, 2), "% of all rounds\n")

cat("\n=== STRATEGY EFFECTIVENESS ===\n")
if (total_forfeits_p1 > 0 || total_forfeits_p2 > 0) {
  cat("The forfeit strategy is being applied successfully!\n")
  cat("Players are strategically giving up weak cards when in a disadvantageous position.\n")
} else {
  cat("No forfeits occurred in these games - strategy conditions may not have been met.\n")
}
