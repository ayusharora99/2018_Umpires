# __DATE__ : Giants vs __OPPONENT__ : __UMPIRE__

# __NUMBER OF__ pitches were called strikes/balls

# The robot-ump called __#__ of those pitches as called strikes & __#__ as balls

# __UMPIRE__ called __#__ of those pitches as called strikes & __#__ as balls 

# Accuracy: __%

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# https://pitchrx.cpsievert.me/ 
library(pitchRx)


# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Umpire_Name <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Umpire_Name_8:27.18.csv", header=TRUE)
Angels <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Angels.csv")
Astros <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Astros.csv")
Athletics <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Athletics.csv")
Bluejays <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Bluejays.csv")
Braves <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Braves.csv")
Brewers <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Brewers.csv")
Cardinals <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Cardinals.csv")
Cubs <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Cubs.csv")
Diamondbacks <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Diamondbacks.csv")
Dodgers <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Dodgers.csv")
Giants <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Giants.csv")
Indians <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Indians.csv")
Mariners <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mariners.csv")
Marlins <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Marlins.csv")
Mets <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mets.csv")
Nationals <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Nationals.csv")
Orioles <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Orioles.csv")
Padres <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Padres.csv")
Phillies <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Phillies.csv")
Pirates <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Pirates.csv")
Rangers <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Rangers.csv")
Rays <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Rays.csv")
RedSox <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/RedSox.csv")
Reds <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Reds.csv")
Rockies <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Rockies.csv")
Royals <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Royals.csv")
Tigers <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Tigers.csv")
Twins <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Twins.csv")
WhiteSox <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/WhiteSox.csv")
Yankees <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Yankees.csv")

season = rbind(Angels,Astros,Athletics,Bluejays,Braves,Brewers,Cardinals,Cubs,Diamondbacks,Dodgers,Giants,Indians,Mariners,Mets,Marlins,Nationals,Orioles,Padres,Phillies,Pirates,Rangers,Rays,RedSox,Reds,Rockies,Royals,Tigers,Twins,WhiteSox,Yankees)



# Getting Familiar With Dataset & removing any NULL values
dim(Umpire_Name)
names(Umpire_Name)
table(is.na(Umpire_Name))
Umpire_Name = Umpire_Name[,colSums(is.na(Umpire_Name)) == 0] 
dim(Umpire_Name)
table(is.na(Umpire_Name))

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Umpire_Name = Umpire_Name[ , !(names(Umpire_Name) %in% drops)]
dim(Umpire_Name)

# Splitting data into Training (80% of data) & Testing (20% of data) sets, Try Cross Validation here
Umpire_Name_train = Umpire_Name[0:(0.8 * nrow(Umpire_Name)),]
dim(Umpire_Name_train)
prop.table(table(Umpire_Name_train$type))
Umpire_Name_test = Umpire_Name[(0.8*nrow(Umpire_Name)):nrow(Umpire_Name),]
dim(Umpire_Name_test)
prop.table(table(Umpire_Name_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Umpire_Name_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Umpire_Name_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Umpire_Name_test$type))

# Subset for Borderline Calls
Umpire_Name$Borderline = ifelse(((abs(Umpire_Name$plate_x)> 0.748) & (abs(Umpire_Name$plate_x)<0.914)) 
                               & (((Umpire_Name$plate_z > Umpire_Name$sz_top-0.83) & (Umpire_Name$plate_z < Umpire_Name$sz_top+0.83))
                                  | (((Umpire_Name$plate_z)<Umpire_Name$sz_bot+0.83) & ((Umpire_Name$plate_z) > Umpire_Name$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Umpire_Name_Strikes = subset(Umpire_Name, Umpire_Name$type == "S")
Umpire_Name_Balls = subset(Umpire_Name, Umpire_Name$type == "B")

# Borderline
Umpire_Name_Borderline = subset(Umpire_Name, Umpire_Name$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Umpire_Name_Strikes$AdjustedCall = ifelse((Umpire_Name_Strikes$plate_x < 0.833 & Umpire_Name_Strikes$plate_x > -0.833) & (Umpire_Name_Strikes$plate_z > Umpire_Name_Strikes$sz_bot & Umpire_Name_Strikes$plate_z < Umpire_Name_Strikes$sz_top), 'S', 'B')
table(Umpire_Name_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Umpire_Name_Balls$AdjustedCall = ifelse((Umpire_Name_Balls$plate_x > 0.833 | Umpire_Name_Balls$plate_x < -0.833)|(Umpire_Name_Balls$plate_z < Umpire_Name_Balls$sz_bot | Umpire_Name_Balls$plate_z > Umpire_Name_Balls$sz_top),'B','S')
table(Umpire_Name_Balls$AdjustedCall)

# Borderline
Umpire_Name_Borderline$AdjustedCall = ifelse((Umpire_Name_Borderline$plate_x < 0.833 & Umpire_Name_Borderline$plate_x > -0.833) & (Umpire_Name_Borderline$plate_z > Umpire_Name_Borderline$sz_bot & Umpire_Name_Borderline$plate_z < Umpire_Name_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Umpire_Name_AdjustedCalls = rbind(Umpire_Name_Strikes,Umpire_Name_Balls)
Umpire_Name_AdjustedCalls$OnFieldRuling = ifelse(Umpire_Name_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Umpire_Name)
plot(tree_model)
text(tree_model, use.n = T)

# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Umpire_Name_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Umpire_Name_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Umpire_Name_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Umpire_Name_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Umpire_Name_AdjustedCalls$Call = ifelse( ((Umpire_Name_AdjustedCalls$type == 'B') & ( (Umpire_Name_AdjustedCalls$AdjustedCall == "B") | (Umpire_Name_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect" )
Umpire_Name_AdjustedCalls$Call = ifelse( ((Umpire_Name_AdjustedCalls$type == 'S') & ((Umpire_Name_AdjustedCalls$AdjustedCall == "S") | (Umpire_Name_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect")

# InCorrect Calls
Umpire_Name_AdjustedCalls$Call = ifelse( ( (Umpire_Name_AdjustedCalls$type == 'B') & ((Umpire_Name_AdjustedCalls$AdjustedCall == "S") & (Umpire_Name_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")
Umpire_Name_AdjustedCalls$Call = ifelse( ( (Umpire_Name_AdjustedCalls$type == 'S') & ((Umpire_Name_AdjustedCalls$AdjustedCall == "B") & (Umpire_Name_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")

table(Umpire_Name_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Umpire_Name_Incorrect = subset(Umpire_Name_AdjustedCalls, Umpire_Name_AdjustedCalls$Call == "I")
print(Umpire_Name_Incorrect$player_name)
print(Umpire_Name_Incorrect$AdjustedCall)



# Getting Familiar With Dataset & removing any NULL values
dim(season)
names(season)
table(is.na(season))
season = season[,colSums(is.na(season)) == 0] # Dangerous
table(is.na(season))

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
season = season[ , !(names(season) %in% drops)]
dim(season)

# Splitting season data into Training (80% of data) & Testing (20% of data) sets
season_train = season[0:(0.8 * nrow(season)),]
dim(season_train)
prop.table(table(season_train$type))
season_test = season[(0.8*nrow(season)):nrow(season),]
dim(season_test)
prop.table(table(season_test$type))

# Subset for Borderline Calls for season
season$Borderline = ifelse(((abs(season$plate_x)> 0.748) & (abs(season$plate_x)<0.914)) 
                                  & (((season$plate_z > season$sz_top-0.83) & (season$plate_z < season$sz_top+0.83))
                                     | (((season$plate_z)<season$sz_bot+0.83) & ((season$plate_z) > season$sz_bot-0.83))), 'T','F')
# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls for season
# Seperate Ball & Strike Types
season_Strikes = subset(season, season$type == "S")
season_Balls = subset(season, season$type == "B")

# Borderline for season
season_Borderline = subset(season, season$Borderline == "T") 

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
season_Strikes$AdjustedCall = ifelse((season_Strikes$plate_x < 0.833 & season_Strikes$plate_x > -0.833) & (season_Strikes$plate_z > season_Strikes$sz_bot & season_Strikes$plate_z < season_Strikes$sz_top), 'S', 'B')
table(season_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
season_Balls$AdjustedCall = ifelse((season_Balls$plate_x > 0.833 | season_Balls$plate_x < -0.833)|(season_Balls$plate_z < season_Balls$sz_bot | season_Balls$plate_z > season_Balls$sz_top),'B','S')
table(season_Balls$AdjustedCall)

# Borderline
season_Borderline$AdjustedCall = ifelse((season_Borderline$plate_x < 0.833 & season_Borderline$plate_x > -0.833) & (season_Borderline$plate_z > season_Borderline$sz_bot & season_Borderline$plate_z < season_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
season_AdjustedCalls = rbind(season_Strikes,season_Balls)
season_AdjustedCalls$OnFieldRuling = ifelse(season_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = season)
plot(tree_model)
text(tree_model, use.n = T)

# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = season_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,season_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = season_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,season_Borderline$AdjustedCall))

# Correct Calls
season_AdjustedCalls$Call = ifelse( ((season_AdjustedCalls$type == 'B') & ( (season_AdjustedCalls$AdjustedCall == "B") | (season_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect" )
season_AdjustedCalls$Call = ifelse( ((season_AdjustedCalls$type == 'S') & ((season_AdjustedCalls$AdjustedCall == "S") | (season_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect")

# InCorrect Calls
season_AdjustedCalls$Call = ifelse( ( (season_AdjustedCalls$type == 'B') & ((season_AdjustedCalls$AdjustedCall == "S") & (season_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")
season_AdjustedCalls$Call = ifelse( ( (season_AdjustedCalls$type == 'S') & ((season_AdjustedCalls$AdjustedCall == "B") & (season_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")

table(season_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
season_Incorrect = subset(season_AdjustedCalls, season_AdjustedCalls$Call == "I")
print(table(season_Incorrect$player_name))
print(season_Incorrect$AdjustedCall)

