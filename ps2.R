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

# vote.choice('Clinton')
# vote.choice('Trump')
# vote.choice('Other')
# vote.choice('a')


# part 4
install.packages('fivethirtyeight')
library(fivethirtyeight)
head(cabinet_turnover)




# description: gets the number of days a president served in office
# input: president's name as a string
# output: number of days the president served in office as a numeric
get.term.time<-function(president){
  presidents<-unique(cabinet_turnover$president)
  time<-c(1461,2922,1461,2922,2922,2922,1105)
  # create dictionary in the form of a dataframe to get serving time of each president
  term.time<-data.frame('president'=presidents, 'time'=time)
  return(term.time[term.time$president==president,'time'])
}


appoint<-function(president.name){
  mask<-cabinet_turnover$president==president.name # create mask to filter out all entries that aren't of the president passed in the argument
  data<-cabinet_turnover[mask,] # apply mask
  data<-data[!is.na(data$days),] # remove entries that have have no length
  
  avg.days<-mean(data$days) # get average time for oppointee
  proportion<-avg.days/get.term.time(president.name) # divide it by number of days the president served in office
}

# a<-appoint('Reagan')
# a


head(congress_age)
era_mask<-congress_age[,'state'] == 'TX'
data<-congress_age[era_mask,]
data
mean(data$age)
# part 5
congress_stats<-function(group.var){

  groups<-unique(congress_age[,group.var])
  df<-data.frame('group'=groups, 'avg.age'=NA) # create data frame to store data
  
  for(group in groups){
    group_mask<-congress_age[,group.var] == group # mask to only get entries in the era
    data<-congress_age[group_mask,] # apply the mask
    avg.age<-mean(data$age)
    df[df$group==group,'avg.age']<-avg.age # get all entries in the era and set the average age to the computed value
  }
  return(df)

}
congress_stats('state')

