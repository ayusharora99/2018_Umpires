# 10/3/18 : A's vs Yankees : Jim Wolfe

# 155 pitches were called strikes/balls

# The robot-ump called 45 of those pitches as called strikes & 110 as balls

# Jim Wolfe called 50 of those pitches as called strikes & 105 as balls 

# Accuracy: 95%
# Borderline Pitch Accuracy: 83%
# 151 Correct vs 4 Incorrect Calls

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Jim_Wolfe <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jim_Wolfe_10:3:18.csv")
giants_season <- read.csv("~/Downloads/savant_data (1).csv")# Packages needed for Analysis
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Jim_Wolfe)
names(Jim_Wolfe)
table(is.na(Jim_Wolfe))
colSums(is.na(Jim_Wolfe)) 
Jim_Wolfe = Jim_Wolfe[,colSums(is.na(Jim_Wolfe)) == 0] 
dim(Jim_Wolfe)
table(is.na(Jim_Wolfe))

# Getting Familiar With Dataset & removing any NULL values
dim(giants_season)
names(giants_season)
table(is.na(giants_season))
giants_season = giants_season[,colSums(is.na(giants_season)) == 0] 
table(is.na(giants_season))

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jim_Wolfe = Jim_Wolfe[ , !(names(Jim_Wolfe) %in% drops)]
dim(Jim_Wolfe)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
giants_season = giants_season[ , !(names(giants_season) %in% drops)]
dim(giants_season)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Jim_Wolfe_train = Jim_Wolfe[0:(0.8 * nrow(Jim_Wolfe)),]
dim(Jim_Wolfe_train)
prop.table(table(Jim_Wolfe_train$type))
Jim_Wolfe_test = Jim_Wolfe[(0.8*nrow(Jim_Wolfe)):nrow(Jim_Wolfe),]
dim(Jim_Wolfe_test)
prop.table(table(Jim_Wolfe_test$type))

# Splitting giants_season data into Training (80% of data) & Testing (20% of data) sets
giants_season_train = giants_season[0:(0.8 * nrow(giants_season)),]
dim(giants_season_train)
prop.table(table(giants_season_train$type))
giants_season_test = giants_season[(0.8*nrow(giants_season)):nrow(giants_season),]
dim(giants_season_test)
prop.table(table(giants_season_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jim_Wolfe_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Jim_Wolfe_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Jim_Wolfe_test$type))

# Creating Decision Tree to Predict Umpire's Call for All 2018 pitches
tree_model_2018  = rpart(type~., data = giants_season)
plot(tree_model_2018)
text(tree_model_2018,use.n = T)

# Testing Decision Tree with 2018 Data
Prediction_UMP_2018<-predict(tree_model_2018, newdata = giants_season, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP_2018, giants_season$type))

# Subset for Borderline Calls
Jim_Wolfe$Borderline = ifelse(((abs(Jim_Wolfe$plate_x)> 0.748) & (abs(Jim_Wolfe$plate_x)<0.914)) 
                                & (((Jim_Wolfe$plate_z > Jim_Wolfe$sz_top-0.83) & (Jim_Wolfe$plate_z < Jim_Wolfe$sz_top+0.83))
                                   | (((Jim_Wolfe$plate_z)<Jim_Wolfe$sz_bot+0.83) & ((Jim_Wolfe$plate_z) > Jim_Wolfe$sz_bot-0.83))), 'T','F')
# Subset for Borderline Calls for giants_season
giants_season$Borderline = ifelse(((abs(giants_season$plate_x)> 0.748) & (abs(giants_season$plate_x)<0.914)) 
                              & (((giants_season$plate_z > giants_season$sz_top-0.83) & (giants_season$plate_z < giants_season$sz_top+0.83))
                                 | (((giants_season$plate_z)<giants_season$sz_bot+0.83) & ((giants_season$plate_z) > giants_season$sz_bot-0.83))), 'T','F')
# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Jim_Wolfe_Strikes = subset(Jim_Wolfe, Jim_Wolfe$type == "S")
Jim_Wolfe_Balls = subset(Jim_Wolfe, Jim_Wolfe$type == "B")

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls for giants_season
# Seperate Ball & Strike Types
giants_season_Strikes = subset(giants_season, giants_season$type == "S")
giants_season_Balls = subset(giants_season, giants_season$type == "B")

# Borderline
Jim_Wolfe_Borderline = subset(Jim_Wolfe, Jim_Wolfe$Borderline == "T")

# Borderline for giants_season
giants_season_Borderline = subset(giants_season, giants_season$Borderline == "T") 

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jim_Wolfe_Strikes$AdjustedCall = ifelse((Jim_Wolfe_Strikes$plate_x < 0.833 & Jim_Wolfe_Strikes$plate_x > -0.833) & (Jim_Wolfe_Strikes$plate_z > Jim_Wolfe_Strikes$sz_bot & Jim_Wolfe_Strikes$plate_z < Jim_Wolfe_Strikes$sz_top), 'S', 'B')
table(Jim_Wolfe_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
giants_season_Strikes$AdjustedCall = ifelse((giants_season_Strikes$plate_x < 0.833 & giants_season_Strikes$plate_x > -0.833) & (giants_season_Strikes$plate_z > giants_season_Strikes$sz_bot & giants_season_Strikes$plate_z < giants_season_Strikes$sz_top), 'S', 'B')
table(giants_season_Strikes$AdjustedCall)


# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jim_Wolfe_Balls$AdjustedCall = ifelse((Jim_Wolfe_Balls$plate_x > 0.833 | Jim_Wolfe_Balls$plate_x < -0.833)|(Jim_Wolfe_Balls$plate_z < Jim_Wolfe_Balls$sz_bot | Jim_Wolfe_Balls$plate_z > Jim_Wolfe_Balls$sz_top),'B','S')
table(Jim_Wolfe_Balls$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
giants_season_Balls$AdjustedCall = ifelse((giants_season_Balls$plate_x > 0.833 | giants_season_Balls$plate_x < -0.833)|(giants_season_Balls$plate_z < giants_season_Balls$sz_bot | giants_season_Balls$plate_z > giants_season_Balls$sz_top),'B','S')
table(giants_season_Balls$AdjustedCall)


# Borderline
Jim_Wolfe_Borderline$AdjustedCall = ifelse((Jim_Wolfe_Borderline$plate_x < 0.833 & Jim_Wolfe_Borderline$plate_x > -0.833) & (Jim_Wolfe_Borderline$plate_z > Jim_Wolfe_Borderline$sz_bot & Jim_Wolfe_Borderline$plate_z < Jim_Wolfe_Borderline$sz_top), 'S', 'B')

# Borderline
giants_season_Borderline$AdjustedCall = ifelse((giants_season_Borderline$plate_x < 0.833 & giants_season_Borderline$plate_x > -0.833) & (giants_season_Borderline$plate_z > giants_season_Borderline$sz_bot & giants_season_Borderline$plate_z < giants_season_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Jim_Wolfe_AdjustedCalls = rbind(Jim_Wolfe_Strikes,Jim_Wolfe_Balls)
Jim_Wolfe_AdjustedCalls$OnFieldRuling = ifelse(Jim_Wolfe_AdjustedCalls$type == "S","S","B")

# Merge to create new dataset
giants_season_AdjustedCalls = rbind(giants_season_Strikes,giants_season_Balls)
giants_season_AdjustedCalls$OnFieldRuling = ifelse(giants_season_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Jim_Wolfe)
plot(tree_model)
text(tree_model, use.n = T)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = giants_season)
plot(tree_model)
text(tree_model, use.n = T)

# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Jim_Wolfe_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Jim_Wolfe_AdjustedCalls$AdjustedCall))

# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = giants_season_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,giants_season_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Jim_Wolfe_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Jim_Wolfe_Borderline$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = giants_season_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,giants_season_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Jim_Wolfe_AdjustedCalls$Call = ifelse( ((Jim_Wolfe_AdjustedCalls$type == 'B') & ( (Jim_Wolfe_AdjustedCalls$AdjustedCall == "B") | (Jim_Wolfe_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect" )
Jim_Wolfe_AdjustedCalls$Call = ifelse( ((Jim_Wolfe_AdjustedCalls$type == 'S') & ((Jim_Wolfe_AdjustedCalls$AdjustedCall == "S") | (Jim_Wolfe_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect")

# Correct Calls
giants_season_AdjustedCalls$Call = ifelse( ((giants_season_AdjustedCalls$type == 'B') & ( (giants_season_AdjustedCalls$AdjustedCall == "B") | (giants_season_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect" )
giants_season_AdjustedCalls$Call = ifelse( ((giants_season_AdjustedCalls$type == 'S') & ((giants_season_AdjustedCalls$AdjustedCall == "S") | (giants_season_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect")

# InCorrect Calls
Jim_Wolfe_AdjustedCalls$Call = ifelse( ( (Jim_Wolfe_AdjustedCalls$type == 'B') & ((Jim_Wolfe_AdjustedCalls$AdjustedCall == "S") & (Jim_Wolfe_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")
Jim_Wolfe_AdjustedCalls$Call = ifelse( ( (Jim_Wolfe_AdjustedCalls$type == 'S') & ((Jim_Wolfe_AdjustedCalls$AdjustedCall == "B") & (Jim_Wolfe_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")

table(Jim_Wolfe_AdjustedCalls$Call)

# InCorrect Calls
giants_season_AdjustedCalls$Call = ifelse( ( (giants_season_AdjustedCalls$type == 'B') & ((giants_season_AdjustedCalls$AdjustedCall == "S") & (giants_season_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")
giants_season_AdjustedCalls$Call = ifelse( ( (giants_season_AdjustedCalls$type == 'S') & ((giants_season_AdjustedCalls$AdjustedCall == "B") & (giants_season_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")

table(giants_season_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Jim_Wolfe_Incorrect = subset(Jim_Wolfe_AdjustedCalls, Jim_Wolfe_AdjustedCalls$Call == "I")
print(Jim_Wolfe_Incorrect$player_name)
print(Jim_Wolfe_Incorrect$AdjustedCall)


# Which Pitchers Recieved the InCorrect Calls
giants_season_Incorrect = subset(giants_season_AdjustedCalls, giants_season_AdjustedCalls$Call == "I")
print(table(giants_season_Incorrect$player_name))
print(giants_season_Incorrect$AdjustedCall)

