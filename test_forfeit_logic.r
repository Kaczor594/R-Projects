# Test the forfeit logic directly
source("war_forfeit_strategy_demo.r")

# Create a test scenario where forfeit should trigger
test_forfeit_logic <- function() {
  cat("=== TESTING FORFEIT LOGIC DIRECTLY ===\n")
  
  # Create a scenario where Player 1 should forfeit
  # Player 1: 20 cards, 1 Ace, plays a 5
  # Player 2: 32 cards
  
  player1_hand <- data.frame(
    Value = c(rep("5", 19), "A"),
    Suit = rep("Hearts", 20),
    Rank = c(rep(4, 19), 13)
  )
  
  player2_hand <- data.frame(
    Value = rep("K", 32),
    Suit = rep("Spades", 32), 
    Rank = rep(12, 32)
  )
  
  # Test card to play (rank 4 = card value 5)
  card_rank <- 4
  
  # Check forfeit conditions
  total_cards <- nrow(player1_hand) + nrow(player2_hand)
  player_cards <- nrow(player1_hand)
  player_aces <- count_aces(player1_hand)
  
  cat("Player 1 cards:", player_cards, "\n")
  cat("Total cards:", total_cards, "\n")
  cat("Player 1 aces:", player_aces, "\n")
  cat("Card rank to play:", card_rank, "\n")
  
  has_fewer_than_half <- player_cards < (total_cards / 2)
  has_fewer_than_2_aces <- player_aces < 2
  plays_card_lower_than_9 <- card_rank < 9
  
  cat("Has fewer than half cards:", has_fewer_than_half, "\n")
  cat("Has fewer than 2 aces:", has_fewer_than_2_aces, "\n")
  cat("Plays card lower than 9:", plays_card_lower_than_9, "\n")
  
  should_forfeit_result <- should_forfeit(player1_hand, player2_hand, card_rank)
  cat("Should forfeit:", should_forfeit_result, "\n")
  
  if (should_forfeit_result) {
    cat("SUCCESS: Forfeit logic is working correctly!\n")
  } else {
    cat("ISSUE: Forfeit logic is not working as expected.\n")
  }
  
  return(should_forfeit_result)
}

# Run the test
test_forfeit_logic()
