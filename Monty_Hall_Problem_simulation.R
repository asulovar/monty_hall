# Simulates the two strategies for playing Monty Hall:
# Strategy 1: player does not change their pick
# Strategy 2: player does change their pick

# Example run: game_results <- monty_hall_simulation()

# Source function

monty_hall_simulation <- function(games_per_round=seq(from=0,to=1000,by=100)){
  # Initialize empty data frame for record-keeping of each "round"
  total_games <- data.frame(Games_total=0,
                            Strategy_1_NoChange_CarPercent=0,
                            Strategy_2_Change_CarPercent=0)
  
  # Loop through different numbers of games being played at each round
  for(max_games in games_per_round){
    
    # Initialize empty dataframe
    output_dataframe <- data.frame(Game_nr=0,
                                   Strategy_1_NoChange=0,
                                   Strategy_2_Change=0)
    
    for(i in 1:max_games){
      # Simulate three doors
      alldoors <- sample(x = c("car","goat","empty"),size = 3)
      
      # make a pick
      random_pick_index <- sample(size=1,x = 1:3)
      random_pick <- alldoors[random_pick_index]
      
      
      # Show empty door
      # Which doors are the only ones the host can show:
      # What's empty?
      
      availdoors <- alldoors[-random_pick_index]
      showdoor_index <- which(availdoors!="car")[1]
      showdoor <- availdoors[showdoor_index]
      
      # strategy 1: no change
      nochange_final <- random_pick
      
      
      # strategy 2: change
      showdoor_index <- which(alldoors==showdoor)
      change_index <- setdiff(x = 1:3,y = unique(c(showdoor_index,random_pick_index)))
      change_final <- alldoors[change_index]
      
      
      # Debugging notes
      #paste0("I pick: ",random_pick)
      #paste0("The Host shows: ",showdoor)
      #paste0("Strategy 1:",nochange_final)
      #paste0("Strategy 2:",change_final)
      
      
      output_dataframe_slice <- data.frame(Game_nr=i,
                                           Strategy_1_NoChange=nochange_final,
                                           Strategy_2_Change=change_final)
      
      output_dataframe <- rbind(output_dataframe_slice,output_dataframe)
    }
    # count successes for each strategy
    Strategy_1_NoChange_CarPercent <- as.numeric(table(output_dataframe$Strategy_1_NoChange=="car")[2]/max_games)
    Strategy_2_Change_CarPercent <- as.numeric(table(output_dataframe$Strategy_2_Change=="car")[2]/max_games)
    
    total_games_slice <- data.frame(Games_total=max_games,
                                    Strategy_1_NoChange_CarPercent=Strategy_1_NoChange_CarPercent,
                                    Strategy_2_Change_CarPercent=Strategy_2_Change_CarPercent)
    
    total_games <- rbind(total_games_slice,total_games)
    
  }
  total_games <- total_games[total_games$Games_total>0,]
  return(total_games)
}  


# Compare success rate of each strategy
plot(game_results$Games_total, game_results$Strategy_1_NoChange_CarPercent,
     type = "o", col = "red",  ylim = c(0, 1),
     main = " Monty Hall: success rates for two strategies",bty='n',
     xlab="Number of games played",ylab="Success rate (%)")

par(new=T); plot(game_results$Games_total, game_results$Strategy_2_Change_CarPercent,
     type = "o", col = "green", ylab = "", ylim = c(0, 1),
     main = "",bty='n',xlab="")

abline(h=1/3,lty=2,lwd=0.7)
abline(h=2/3,lty=2,lwd=0.7)

legend(100, 1, legend = c("Keep door", "Change door"), col = c("red", "green"), lty = 1:1, cex = 0.8,
       border = F,bty='n')


