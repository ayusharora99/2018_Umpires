# 9/26/18 : Giants vs Padres : Ryan Additon

# 125 pitches were called strikes/balls

# The robot-ump called 32 of those pitches as called strikes & 93 as balls

# Ryan Additon called 36 of those pitches as called strikes & 89 as balls 

# Accuracy: 90%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Ryan_Additon <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Ryan_Additon_9:26:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Ryan_Additon)
names(Ryan_Additon)
is.na(Ryan_Additon)
colSums(is.na(Ryan_Additon)) 
Ryan_Additon = Ryan_Additon[,colSums(is.na(Ryan_Additon)) == 0] 
dim(Ryan_Additon)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Ryan_Additon = Ryan_Additon[ , !(names(Ryan_Additon) %in% drops)]
dim(Ryan_Additon)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Ryan_Additon_train = Ryan_Additon[0:(0.8 * nrow(Ryan_Additon)),]
dim(Ryan_Additon_train)
prop.table(table(Ryan_Additon_train$type))
Ryan_Additon_test = Ryan_Additon[(0.8*nrow(Ryan_Additon)):nrow(Ryan_Additon),]
dim(Ryan_Additon_test)
prop.table(table(Ryan_Additon_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Ryan_Additon_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Ryan_Additon_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Ryan_Additon_test$type))

# Subset for Borderline Calls
Ryan_Additon$Borderline = ifelse(((abs(Ryan_Additon$plate_x)> 0.748) & (abs(Ryan_Additon$plate_x)<0.914)) 
                                & (((Ryan_Additon$plate_z > Ryan_Additon$sz_top-0.83) & (Ryan_Additon$plate_z < Ryan_Additon$sz_top+0.83))
                                   | (((Ryan_Additon$plate_z)<Ryan_Additon$sz_bot+0.83) & ((Ryan_Additon$plate_z) > Ryan_Additon$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Ryan_Additon_Strikes = subset(Ryan_Additon, Ryan_Additon$type == "S")
Ryan_Additon_Balls = subset(Ryan_Additon, Ryan_Additon$type == "B")

# Borderline
Ryan_Additon_Borderline = subset(Ryan_Additon, Ryan_Additon$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Ryan_Additon_Strikes$AdjustedCall = ifelse((Ryan_Additon_Strikes$plate_x < 0.833 & Ryan_Additon_Strikes$plate_x > -0.833) & (Ryan_Additon_Strikes$plate_z > Ryan_Additon_Strikes$sz_bot & Ryan_Additon_Strikes$plate_z < Ryan_Additon_Strikes$sz_top), 'S', 'B')
table(Ryan_Additon_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Ryan_Additon_Balls$AdjustedCall = ifelse((Ryan_Additon_Balls$plate_x > 0.833 | Ryan_Additon_Balls$plate_x < -0.833)|(Ryan_Additon_Balls$plate_z < Ryan_Additon_Balls$sz_bot | Ryan_Additon_Balls$plate_z > Ryan_Additon_Balls$sz_top),'B','S')
table(Ryan_Additon_Balls$AdjustedCall)

# Borderline
Ryan_Additon_Borderline$AdjustedCall = ifelse((Ryan_Additon_Borderline$plate_x < 0.833 & Ryan_Additon_Borderline$plate_x > -0.833) & (Ryan_Additon_Borderline$plate_z > Ryan_Additon_Borderline$sz_bot & Ryan_Additon_Borderline$plate_z < Ryan_Additon_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Ryan_Additon_AdjustedCalls = rbind(Ryan_Additon_Strikes,Ryan_Additon_Balls)
Ryan_Additon_AdjustedCalls$OnFieldRuling = ifelse(Ryan_Additon_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Ryan_Additon)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Ryan_Additon_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Ryan_Additon_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Ryan_Additon_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Ryan_Additon_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Ryan_Additon_AdjustedCalls$Call = ifelse( ((Ryan_Additon_AdjustedCalls$type == 'B') & ( (Ryan_Additon_AdjustedCalls$AdjustedCall == "B") | (Ryan_Additon_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Ryan_Additon_AdjustedCalls$Call = ifelse( ((Ryan_Additon_AdjustedCalls$type == 'S') & ((Ryan_Additon_AdjustedCalls$AdjustedCall == "S") | (Ryan_Additon_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Ryan_Additon_AdjustedCalls$Call = ifelse( ( (Ryan_Additon_AdjustedCalls$type == 'B') & ((Ryan_Additon_AdjustedCalls$AdjustedCall == "S") & (Ryan_Additon_AdjustedCalls$Borderline == "F") ) ), "I","C")
Ryan_Additon_AdjustedCalls$Call = ifelse( ( (Ryan_Additon_AdjustedCalls$type == 'S') & ((Ryan_Additon_AdjustedCalls$AdjustedCall == "B") & (Ryan_Additon_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Ryan_Additon_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Ryan_Additon_Incorrect = subset(Ryan_Additon_AdjustedCalls, Ryan_Additon_AdjustedCalls$Call == "I")
print(Ryan_Additon_Incorrect$player_name)
print(Ryan_Additon_Incorrect$AdjustedCall)
