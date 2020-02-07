# part 1
for(i in seq(1,7)){
  print(i^3)
}


# part 2
max.rolls<-1000
game.roll.count<-0 # keeps track of how many times the dice have been rolled in the current game
games.completed<-0 # keeps track of how many games have been completed
total.rolls<-0 # keeps track of how total rolls done in all of the completed games
#(don't want to divide max.rolls by games completed because we might have not completed a game on the 1000 roll)


set.seed(14)
for(i in seq(1,max.rolls)){
  roll<-sample(1:6, 2, replace=TRUE) # simulate rolling two dice
  #print(paste('roll:', roll))
  game.roll.count<-game.roll.count+1
  if(game.roll.count==1 & sum(roll) %in% seq(8,12)){
    # if it's the first roll of the game and you get between 8 and 12 then end the game
    games.completed<-games.completed+1
    total.rolls<-i
    #print(paste('finished on first round.', 'total rolls:', total.rolls, 'games.completed:', games.completed))
    game.roll.count<-0
  }
  else if(game.roll.count!=1 & sum(roll) %in% c(2,6)){
    # roll sums to 2 or 6 and it's not the first roll
    games.completed<-games.completed+1
    #print(paste('finished on round:', i-total.rolls,'total rolls:', i, 'games.completed:', games.completed))
    total.rolls<-i
    game.roll.count<-0
    
  }
}

avg.rolls <- total.rolls / games.completed
print(paste('average number of roll is:', avg.rolls))


# part 3
webData<-url("http://politicaldatascience.com/PDS/Problem%20Sets/Problem%20Set%202/GSS-data.csv")
df <- read.csv(webData)
df[,'pres16']

# COMMENT FUNCTION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
vote.choice<-function(candidate){
  if(candidate %in% c('Clinton', 'Trump', 'Other')){
    if(candidate == 'Other'){
      candidate = 'Other candidate (specify)'
    }
    return(sum(df[,'pres16'] == candidate))
  }
  else{
    print('Please enter either 'Trump' 'Clinton' or 'Other' into the function to return a valid response')
  }
  
}

vote.choice('Clinton')
vote.choice('Trump')
vote.choice('Other')
vote.choice('a')


# part 4
install.packages('fivethirtyeight')
library(fivethirtyeight)
head(cabinet_turnover)


# create dictionary in the form of a dataframe to get serving time of each president
presidents<-unique(cabinet_turnover$president)
class(presidents)

time<-c(1461,2922,1461,2922,2922,2922,1005)
class(time)
length(presidents)
length(time)
term.time<-data.frame('president'=presidents, 'time'=time)
str(term.time)
term.time[term.time$president=='Obama','time']

appoint<-function(president.name){
  mask<-cabinet_turnover$president==president.name # create mask to filter out all entries that aren't of the president passed in the argument
  data<-cabinet_turnover[mask,] # apply mask
  head(data)
}

appoint('Bush 41')
