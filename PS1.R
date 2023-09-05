#Problem 1
#a)
winedata<-read.csv("/Users/cathy/Desktop/wine/wine.csv",header=FALSE)
df<-data.frame(winedata)
print(df)
colnames(df)<-c("class","Alcohol","Malicacid","Ash","Alcalinity_of_ash","Magnesium",
                "Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins",
                "Color_intensity","Hue","OD280/OD315_of_diluted_wines","Proline")
print(df)
#b)
# Load dplyr
library(dplyr)
# Group by count using dplyr
wine_count<-df %>% group_by(class) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
wine_count
#The result matches the number of instances in the wine.names file.
# Convert tibble to df
#df2 <- agg_tbl %>% as.data.frame()
#df2

#c)
df[df$Alcohol==max(df$Alcohol),]#Extract the rows with highest alcohol content.
df[df$Alcohol==min(df$Alcohol),]#Extract the rows with lowest alcohol content.
count(df[df$Magnesium>114, ])#Extract the rows with magnesium higher than 114mg/l then count them.
df2<-df[df$Magnesium>114, ]
count_by_magnesium<- df2 %>% group_by(class) %>% summarise(total_count=n(),.groups = 'drop')
count_by_magnesium

#d)
# Using apply
all.means<-apply(df, 2, mean)
# Using colMeans
all.means<-colMeans(df)
#Applying the same method for 3 different classes.
df3<-df[df$class==1,]
means_c1<-apply(df3, 2, mean)
means_c1<-colMeans(df3)

df4<-df[df$class==2,]
means_c2<-apply(df4, 2, mean)
means_c2<-colMeans(df4)

df5<-df[df$class==3,]
means_c3<-apply(df5, 2, mean)
means_c3<-colMeans(df5)

#Combining these dataframe into one table.
avg_table =cbind.data.frame(all.means,means_c1,means_c2,means_c3)
avg_table

#e)
#t test on level of ashes of class 1 and class 2
t.test(df3$Ash, df4$Ash, var.equal = TRUE)
#t test on level of ashes of class 2 and class 3
t.test(df4$Ash, df5$Ash, var.equal = TRUE)
#t test on level of ashes of class 1 and class 3
t.test(df3$Ash, df5$Ash, var.equal = TRUE)
#Choosing a significant level of 0.01. 
#According the t test results of class 1 and 2, 
#since the p-value=3.493e-05 which is smaller than 0.01, the null hypothesis is rejected, 
#the difference between the means of level of ash in class 1 and 2 is statistically significant.

#According the t test results of class 2 and 3, 
#since the p-value=0.0002293 which is smaller than 0.01, the null hypothesis is rejected, 
#the difference between the means of level of ash in class 2 and 3 is statistically significant.

#According the t test results of class 1 and 3, 
#since the p-value=0.6499 which is greater than 0.01, the null hypothesis is not rejected, 
#the difference between the means of level of ash in class 1 and 3 is not statistically significant.


#Problem 2
#a)
isPerfectPower<-function(number,power) {
  if (number<1||power<2) {
    isPerfect<-FALSE
    return(list(isPerfect=isPerfect,root=NA))
  }
  
  root<-round(number^(1/power)) #calculate the root of the number for the given power 
  #and rounds it to the nearest integer. 
  #This root is then used to check if the number is a perfect power when raised to the specified power.
  
  if (root^power==number) {
    isPerfect<-TRUE
    return(list(isPerfect=isPerfect,root=root))
  } else {
    isPerfect<-FALSE
    return(list(isPerfect=isPerfect,root=root))
  }
}

isPerfectPower(16,2)
isPerfectPower(125,3)

#b)
findRootPower<-function(number) {
  for (power in 2:50) {  # This range of powers to check can be adjusted.
    result <- isPerfectPower(number,power)
    if (result[[1]]) {
      return(paste(number,"=",result[[2]],"^",power))
    }
  }
  return("Input is not a perfect power")
}

findRootPower(27)
findRootPower(13060694016)
findRootPower(7776)
findRootPower(170859375)
findRootPower(58247422)
findRootPower(94143178827)

#Problem 3
#a)
#Sure, here are two R functions that accomplish the tasks you've described. 
#The first function, identify_poker_hand, takes vectors of suits and ranks representing a 5-card hand and returns the name of the poker hand. 
#The second function, deal_poker_round, simulates dealing a round of cards in a game of 5-card stud poker 
#for a specified number of players and displays the hands and their names using the first function.

# Create a function to identify the name of a poker hand.
identify_poker_hand <- function(suits, ranks) {
  # Ensure that suits and ranks are of the same length which according to the requirement, it should be 5.
  if (length(suits) != 5 || length(ranks) != 5) {
    stop("Both suits and ranks should have 5 elements.")
  } #If the length of suits or ranks is not 5, the program will stop and send message to user.
  
  # Create a function to count the frequency of ranks.
  count_ranks <- function(ranks) {
    rank_counts <- table(ranks) #It counts how many times each unique rank appears in the ranks vector and stores this information in the rank_counts variable. 
    return(rank_counts)#Instructs the function to return the rank_counts table as its result. 
  }
  
  # Check for flush, if it is a flush the length for suits has to be 1. 
  is_flush <- length(unique(suits)) == 1
  
  # Check for straight.
  sorted_ranks <- sort(unique(ranks)) # sorted_ranks will contain a sorted vector of unique ranks from the input ranks vector.
  is_straight <- length(sorted_ranks) == 5 && max(sorted_ranks) - min(sorted_ranks) == 4
  #There should be 5 unique ranks for a sorted rank to potentially be a straight. In a straight, the ranks should also have a range of 4.
  #If both of these conditions are true, the variable is_straight will be set to TRUE, indicating that the ranks indeed form a straight. 
  #Otherwise, it will be set to FALSE.
  
   
  # Count rank frequencies.
  rank_counts <- count_ranks(ranks)
  
  # Check for four of a kind, full house, three of a kind, and the number of pairs, based on the frequency of card ranks in the hand, specifically, by using any function.
  has_four_of_a_kind <- any(rank_counts == 4)# In a four-of-a-kind, there should be exactly four cards with the same rank. If this condition is met, has_four_of_a_kind is set to TRUE.
  has_full_house <- any(rank_counts == 3) && any(rank_counts == 2)#The first any checks if there's a rank with a frequency of 3 (three of a kind), and the second any checks if there's another rank with a frequency of 2 (a pair). If both conditions are met, indicating both a three of a kind and a pair, has_full_house is set to TRUE.
  has_three_of_a_kind <- any(rank_counts == 3)#Check if there is any rank with a frequency of 3 in the rank_counts table. If this condition is met, has_three_of_a_kind is set to TRUE.
  num_pairs <- sum(rank_counts == 2)#This line counts the number of pairs in the hand and store the value in num_pairs. 
  
  
  # Determine the poker hand.
  if (is_flush && is_straight) { #checks if the hand is both a flush and a straight.
    if (max(sorted_ranks) == 14) { #If it is, it further checks if the highest rank in the straight is 14, which represents an Ace.
      return("Royal Flush")
    } else {
      return("Straight Flush")
    }# If max(sorted_ranks) == 14 is met, it returns "Royal Flush," indicating the highest-ranking poker hand. Otherwise, it returns "Straight Flush."
  } else if (has_four_of_a_kind) {
    return("Four of a Kind") # This condition checks if there is a four-of-a-kind in the hand. If so, it returns "Four of a Kind."
  } else if (has_full_house) {
    return("Full House") #This condition checks if there is a full house in the hand. If so, it returns "Full House."
  } else if (is_flush) {
    return("Flush") #This condition checks if the hand is a flush. If so, it returns "Flush."
  } else if (is_straight) {
    return("Straight") #This condition checks if the hand is a straight (but not a flush). If so, it returns "Straight."
  } else if (has_three_of_a_kind) {
    return("Three of a Kind") #This condition checks if there is a three-of-a-kind in the hand. If so, it returns "Three of a Kind."
  } else if (num_pairs == 2) {
    return("Two Pair") #This condition checks if there are two pairs in the hand. If so, it returns "Two Pair."
  } else if (num_pairs == 1) {
    return("One Pair") #This condition checks if there is one pair in the hand. If so, it returns "One Pair."
  } else {
    return("High Card") #If none of the above conditions are met, it means the hand is just a "High Card" hand.
  }
}

# Function to simulate dealing a round of 5-card stud poker.
deal_poker_round <- function(num_players) {
  #Set up the basic data structures needed for the poker game.
  # Define suits and ranks
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  ranks <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)  # 11 = Jack, 12 = Queen, 13 = King, 14 = Ace
  
  # Create a deck of cards
  deck <- expand.grid(Suit = suits, Rank = ranks)
  #This line combines predefined suits (Hearts, Diamonds, Clubs, Spades) and ranks (2 to 14) using expand.grid to create a dataframe, representing a complete deck of cards.
  
  # Shuffle the deck
  shuffled_deck <- deck[sample(nrow(deck)), ]
  #Sample() is used to randomly sample rows from the deck data frame.
  #The shuffled deck is stored in the shuffled_deck data frame. It contains the same cards as the original deck but in a random order.
  
  # Deal hands to players
  hands <- list() #Initializes an empty list called hands to store the poker hands of each player.
  for (i in 1:num_players) {
    player_hand <- shuffled_deck[((i - 1) * 5 + 1):(i * 5), ]#Inside the loop, it extracts a 5-card hand from the shuffled deck for the current player (i). 
    hands[[i]] <- player_hand#The extracted player hand is then added to the hands list.
    cat(paste("Player", i, "Hand:\n"))#This line prints a message indicating the player number and their hand displayed.
    print(player_hand)#Displays the 5-card hand for the current player.
    cat("Hand Name:", identify_poker_hand(player_hand$Suit, player_hand$Rank), "\n\n")#This line prints the poker hand name for the current player's hand. 
  }
}

#This code defines the two functions you requested and provides an example of 
#how to use them to simulate a round of 5-card stud poker with a specified number of players.

#b) Check whether the above codes can run without modification in R, and to do so, use some examples to check.
# Example usage:
deal_poker_round(2)  # Simulate a round of 5-card stud poker with 2 players
deal_poker_round(4)  # Simulate a round of 5-card stud poker with 4 players
deal_poker_round(7)  # Simulate a round of 5-card stud poker with 7 players
deal_poker_round(10) # Simulate a round of 5-card stud poker with 10 players
#The code runs without errors for a variety on inputs.

#c) The line-by-line explanation are displayed in comments made after each line in the code in part a. 

#d)
#1.Are the inputs and outputs as described above?
#2.Are the hands valid (e.g. real cards, no duplicates, right number)?
#3.Are the names of the hands correct? (See here if you’re not familiar with poker hands.)
#4.Does it ensure no duplicates in cards across hands? What happens if you ask for more than 10 hands to be dealt (as there are only 52 cards in a standard deck)?