# Create a simulation of the card game War using R
deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  values <- c(2:10, "J", "Q", "K", "A")
  cards <- expand.grid(Value = values, Suit = suits)
  cards$Rank <- match(cards$Value, c(2:10, "J", "Q", "K", "A"))
  return(cards)
}

# Create and examine the deck
my_deck <- deck()
# View the structure of the data frame
str(my_deck)
# View the first few rows
head(my_deck)
# View the last few rows
tail(my_deck)
# Get a summary of the data frame
summary(my_deck)
# View the dimensions (rows and columns)
dim(my_deck)
# View all unique values in each column
unique(my_deck$Value)
unique(my_deck$Suit)
unique(my_deck$Rank)

shuffle_deck <- function(deck) {
  deck[sample(nrow(deck)), ]
}

# Create and examine the shuffled deck
my_shuffled_deck <- shuffle_deck(my_deck)
# View the first few rows
head(my_shuffled_deck)


deal_cards <- function(shuffled_deck) {
  player1 <- shuffled_deck[1:26, ]
  player2 <- shuffled_deck[27:52, ]
  list(player1 = player1, player2 = player2)
}

# Create and examine the dealt hands
hands <- deal_cards(my_shuffled_deck)
player1_hand <- hands$player1
player2_hand <- hands$player2
# View the first few rows of each player's hand
head(player1_hand)
head(player2_hand)
# Summary of each player's hand
summary(player1_hand)
summary(player2_hand)

play_round <- function(player1, player2) {
  card1 <- player1[1, ]
  card2 <- player2[1, ]
  if (card1$Rank > card2$Rank) {
    return("Player 1 wins the round")
  } else if (card1$Rank < card2$Rank) {
    return("Player 2 wins the round")
  } else {
    return("It's a tie")
  }
}

simulate_game <- function() {
  d <- deck()
  shuffled <- shuffle_deck(d)
  hands <- deal_cards(shuffled)
  player1 <- hands$player1
  player2 <- hands$player2
  
  round_num <- 1
  while (nrow(player1) > 0 && nrow(player2) > 0) {
    # Get the top cards
    card1 <- player1[1, ]
    card2 <- player2[1, ]
    
    # Remove the top cards from each hand
    player1 <- player1[-1, ]
    player2 <- player2[-1, ]
    
    # Determine winner and redistribute cards
    if (card1$Rank > card2$Rank) {
      cat("Round", round_num, ": Player 1 wins the round\n")
      # Player 1 wins both cards - add to bottom of their hand
      player1 <- rbind(player1, card1, card2)
    } else if (card1$Rank < card2$Rank) {
      cat("Round", round_num, ": Player 2 wins the round\n")
      # Player 2 wins both cards - add to bottom of their hand
      player2 <- rbind(player2, card2, card1)
    } else {
      # WAR! Both players play 4 more cards
      cat("Round", round_num, ": It's a tie - WAR!\n")
      
      # Initialize cards in play with the original tied cards
      cards_in_play <- rbind(card1, card2)
      
      # Keep warring until there's a winner or someone runs out of cards
      war_continues <- TRUE
      war_count <- 1
      
      while (war_continues) {
        # Check if both players have at least 4 cards for war
        if (nrow(player1) < 4 || nrow(player2) < 4) {
          cat("Not enough cards for war - game ends\n")
          war_continues <- FALSE
          break
        }
        
        cat("WAR #", war_count, "!\n")
        
        # Each player plays 4 more cards (3 face down, 1 face up)
        war_cards1 <- player1[1:4, ]
        war_cards2 <- player2[1:4, ]
        
        # Remove the war cards from each hand
        player1 <- player1[-(1:4), ]
        player2 <- player2[-(1:4), ]
        
        # Add war cards to cards in play
        cards_in_play <- rbind(cards_in_play, war_cards1, war_cards2)
        
        # The 4th card determines the winner
        deciding_card1 <- war_cards1[4, ]
        deciding_card2 <- war_cards2[4, ]
        
        if (deciding_card1$Rank > deciding_card2$Rank) {
          cat("Player 1 wins the war with", deciding_card1$Value, "vs", deciding_card2$Value, "\n")
          cat("Player 1 takes", nrow(cards_in_play), "cards!\n")
          # Player 1 wins all cards in play
          player1 <- rbind(player1, cards_in_play)
          war_continues <- FALSE
        } else if (deciding_card1$Rank < deciding_card2$Rank) {
          cat("Player 2 wins the war with", deciding_card2$Value, "vs", deciding_card1$Value, "\n")
          cat("Player 2 takes", nrow(cards_in_play), "cards!\n")
          # Player 2 wins all cards in play
          player2 <- rbind(player2, cards_in_play)
          war_continues <- FALSE
        } else {
          # Another tie - continue the war
          cat("Another tie! War continues...\n")
          war_count <- war_count + 1
        }
      }
    }
    
    round_num <- round_num + 1
    
    # Optional: limit rounds to prevent infinite games
    if (round_num > 1000) {
      cat("Game ended after 1000 rounds to prevent infinite loop\n")
      break
    }
  }
  
  # Announce final winner
  if (nrow(player1) > nrow(player2)) {
    cat("GAME OVER: Player 1 wins with", nrow(player1), "cards!\n")
  } else if (nrow(player2) > nrow(player1)) {
    cat("GAME OVER: Player 2 wins with", nrow(player2), "cards!\n")
  } else {
    cat("GAME OVER: It's a tie!\n")
  }
}

# Function to simulate many games and collect statistics
simulate_many_games <- function(num_games = 1000) {
  # Initialize tracking variables
  player1_wins <- 0
  player2_wins <- 0
  ties <- 0
  all_game_details <- list()
  
  cat("Simulating", num_games, "games of War...\n")
  
  for (game_num in 1:num_games) {
    if (game_num %% 100 == 0) {
      cat("Completed", game_num, "games\n")
    }
    
    # Initialize game tracking
    game_details <- list(
      game_number = game_num,
      rounds = list(),
      total_rounds = 0,
      wars = 0,
      winner = NA,
      final_cards_player1 = 0,
      final_cards_player2 = 0
    )
    
    # Set up game
    d <- deck()
    shuffled <- shuffle_deck(d)
    hands <- deal_cards(shuffled)
    player1 <- hands$player1
    player2 <- hands$player2
    
    round_num <- 1
    while (nrow(player1) > 0 && nrow(player2) > 0) {
      # Get the top cards
      card1 <- player1[1, ]
      card2 <- player2[1, ]
      
      # Remove the top cards from each hand
      player1 <- player1[-1, ]
      player2 <- player2[-1, ]
      
      # Initialize round details
      round_details <- list(
        round_number = round_num,
        player1_card = paste(card1$Value, "of", card1$Suit),
        player2_card = paste(card2$Value, "of", card2$Suit),
        player1_rank = card1$Rank,
        player2_rank = card2$Rank,
        result = NA,
        war_occurred = FALSE,
        wars_in_round = 0,
        cards_won = 2
      )
      
      # Determine winner and redistribute cards
      if (card1$Rank > card2$Rank) {
        round_details$result <- "Player 1 wins"
        player1 <- rbind(player1, card1, card2)
      } else if (card1$Rank < card2$Rank) {
        round_details$result <- "Player 2 wins"
        player2 <- rbind(player2, card2, card1)
      } else {
        # WAR!
        round_details$war_occurred <- TRUE
        game_details$wars <- game_details$wars + 1
        
        # Initialize cards in play with the original tied cards
        cards_in_play <- rbind(card1, card2)
        
        # Keep warring until there's a winner or someone runs out of cards
        war_continues <- TRUE
        war_count <- 1
        
        while (war_continues) {
          # Check if both players have at least 4 cards for war
          if (nrow(player1) < 4 || nrow(player2) < 4) {
            round_details$result <- "Insufficient cards for war"
            war_continues <- FALSE
            break
          }
          
          round_details$wars_in_round <- war_count
          
          # Each player plays 4 more cards
          war_cards1 <- player1[1:4, ]
          war_cards2 <- player2[1:4, ]
          
          # Remove the war cards from each hand
          player1 <- player1[-(1:4), ]
          player2 <- player2[-(1:4), ]
          
          # Add war cards to cards in play
          cards_in_play <- rbind(cards_in_play, war_cards1, war_cards2)
          
          # The 4th card determines the winner
          deciding_card1 <- war_cards1[4, ]
          deciding_card2 <- war_cards2[4, ]
          
          if (deciding_card1$Rank > deciding_card2$Rank) {
            round_details$result <- "Player 1 wins war"
            player1 <- rbind(player1, cards_in_play)
            war_continues <- FALSE
          } else if (deciding_card1$Rank < deciding_card2$Rank) {
            round_details$result <- "Player 2 wins war"
            player2 <- rbind(player2, cards_in_play)
            war_continues <- FALSE
          } else {
            # Another tie - continue the war
            war_count <- war_count + 1
          }
        }
        
        round_details$cards_won <- nrow(cards_in_play)
      }
      
      # Add round details to game
      game_details$rounds[[round_num]] <- round_details
      round_num <- round_num + 1
      
      # Safety limit
      if (round_num > 10000) {
        break
      }
    }
    
    # Record game results
    game_details$total_rounds <- round_num - 1
    game_details$final_cards_player1 <- nrow(player1)
    game_details$final_cards_player2 <- nrow(player2)
    
    if (nrow(player1) > nrow(player2)) {
      game_details$winner <- "Player 1"
      player1_wins <- player1_wins + 1
    } else if (nrow(player2) > nrow(player1)) {
      game_details$winner <- "Player 2"
      player2_wins <- player2_wins + 1
    } else {
      game_details$winner <- "Tie"
      ties <- ties + 1
    }
    
    # Store game details
    all_game_details[[game_num]] <- game_details
  }
  
  # Print summary statistics
  cat("\n=== SIMULATION RESULTS ===\n")
  cat("Total games:", num_games, "\n")
  cat("Player 1 wins:", player1_wins, "(", round(player1_wins/num_games*100, 1), "%)\n")
  cat("Player 2 wins:", player2_wins, "(", round(player2_wins/num_games*100, 1), "%)\n")
  cat("Ties:", ties, "(", round(ties/num_games*100, 1), "%)\n")
  
  # Calculate additional statistics
  total_rounds <- sum(sapply(all_game_details, function(x) x$total_rounds))
  total_wars <- sum(sapply(all_game_details, function(x) x$wars))
  avg_rounds <- round(total_rounds / num_games, 1)
  avg_wars <- round(total_wars / num_games, 2)
  
  cat("Average rounds per game:", avg_rounds, "\n")
  cat("Total wars across all games:", total_wars, "\n")
  cat("Average wars per game:", avg_wars, "\n")
  cat("War frequency:", round(total_wars/total_rounds*100, 2), "% of rounds\n")
  
  # Return all data
  return(list(
    summary = list(
      total_games = num_games,
      player1_wins = player1_wins,
      player2_wins = player2_wins,
      ties = ties,
      total_rounds = total_rounds,
      total_wars = total_wars,
      avg_rounds_per_game = avg_rounds,
      avg_wars_per_game = avg_wars
    ),
    all_games = all_game_details
  ))
}

# Run the simulation
simulation_results <- simulate_many_games(1000)

# Example: Access details of first game
cat("\n=== FIRST GAME DETAILS ===\n")
first_game <- simulation_results$all_games[[1]]
cat("Winner:", first_game$winner, "\n")
cat("Total rounds:", first_game$total_rounds, "\n")
cat("Wars:", first_game$wars, "\n")
cat("Final cards - Player 1:", first_game$final_cards_player1, ", Player 2:", first_game$final_cards_player2, "\n")

# Show first few rounds of first game
cat("\nFirst 5 rounds:\n")
max_rounds_to_show <- min(5, length(first_game$rounds))
if (max_rounds_to_show > 0) {
  for (i in seq_len(max_rounds_to_show)) {
    round_info <- first_game$rounds[[i]]
    cat("Round", i, ":", round_info$player1_card, "vs", round_info$player2_card, 
        "->", round_info$result, "\n")
  }
}
