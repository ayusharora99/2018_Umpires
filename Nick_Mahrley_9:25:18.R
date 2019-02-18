# 9/25/18 : Giants vs Padres : Nick Mahrley

# 167 pitches were called strikes/balls

# The robot-ump called 45 of those pitches as called strikes & 123 as balls

# Nick Mahrley called 62 of those pitches as called strikes & 105 as balls 

# Accuracy: 90%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Nick_Mahrley <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Nick_Mahrley_9:25:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Nick_Mahrley)
names(Nick_Mahrley)
is.na(Nick_Mahrley)
colSums(is.na(Nick_Mahrley)) 
Nick_Mahrley = Nick_Mahrley[,colSums(is.na(Nick_Mahrley)) == 0] 
dim(Nick_Mahrley)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Nick_Mahrley = Nick_Mahrley[ , !(names(Nick_Mahrley) %in% drops)]
dim(Nick_Mahrley)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Nick_Mahrley_train = Nick_Mahrley[0:(0.8 * nrow(Nick_Mahrley)),]
dim(Nick_Mahrley_train)
prop.table(table(Nick_Mahrley_train$type))
Nick_Mahrley_test = Nick_Mahrley[(0.8*nrow(Nick_Mahrley)):nrow(Nick_Mahrley),]
dim(Nick_Mahrley_test)
prop.table(table(Nick_Mahrley_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Nick_Mahrley_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Nick_Mahrley_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Nick_Mahrley_test$type))

# Subset for Borderline Calls
Nick_Mahrley$Borderline = ifelse(((abs(Nick_Mahrley$plate_x)> 0.748) & (abs(Nick_Mahrley$plate_x)<0.914)) 
                                & (((Nick_Mahrley$plate_z > Nick_Mahrley$sz_top-0.83) & (Nick_Mahrley$plate_z < Nick_Mahrley$sz_top+0.83))
                                   | (((Nick_Mahrley$plate_z)<Nick_Mahrley$sz_bot+0.83) & ((Nick_Mahrley$plate_z) > Nick_Mahrley$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Nick_Mahrley_Strikes = subset(Nick_Mahrley, Nick_Mahrley$type == "S")
Nick_Mahrley_Balls = subset(Nick_Mahrley, Nick_Mahrley$type == "B")

# Borderline
Nick_Mahrley_Borderline = subset(Nick_Mahrley, Nick_Mahrley$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Nick_Mahrley_Strikes$AdjustedCall = ifelse((Nick_Mahrley_Strikes$plate_x < 0.833 & Nick_Mahrley_Strikes$plate_x > -0.833) & (Nick_Mahrley_Strikes$plate_z > Nick_Mahrley_Strikes$sz_bot & Nick_Mahrley_Strikes$plate_z < Nick_Mahrley_Strikes$sz_top), 'S', 'B')
table(Nick_Mahrley_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Nick_Mahrley_Balls$AdjustedCall = ifelse((Nick_Mahrley_Balls$plate_x > 0.833 | Nick_Mahrley_Balls$plate_x < -0.833)|(Nick_Mahrley_Balls$plate_z < Nick_Mahrley_Balls$sz_bot | Nick_Mahrley_Balls$plate_z > Nick_Mahrley_Balls$sz_top),'B','S')
table(Nick_Mahrley_Balls$AdjustedCall)

# Borderline
Nick_Mahrley_Borderline$AdjustedCall = ifelse((Nick_Mahrley_Borderline$plate_x < 0.833 & Nick_Mahrley_Borderline$plate_x > -0.833) & (Nick_Mahrley_Borderline$plate_z > Nick_Mahrley_Borderline$sz_bot & Nick_Mahrley_Borderline$plate_z < Nick_Mahrley_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Nick_Mahrley_AdjustedCalls = rbind(Nick_Mahrley_Strikes,Nick_Mahrley_Balls)
Nick_Mahrley_AdjustedCalls$OnFieldRuling = ifelse(Nick_Mahrley_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Nick_Mahrley)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Nick_Mahrley_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Nick_Mahrley_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Nick_Mahrley_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Nick_Mahrley_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Nick_Mahrley_AdjustedCalls$Call = ifelse( ((Nick_Mahrley_AdjustedCalls$type == 'B') & ( (Nick_Mahrley_AdjustedCalls$AdjustedCall == "B") | (Nick_Mahrley_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Nick_Mahrley_AdjustedCalls$Call = ifelse( ((Nick_Mahrley_AdjustedCalls$type == 'S') & ((Nick_Mahrley_AdjustedCalls$AdjustedCall == "S") | (Nick_Mahrley_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Nick_Mahrley_AdjustedCalls$Call = ifelse( ( (Nick_Mahrley_AdjustedCalls$type == 'B') & ((Nick_Mahrley_AdjustedCalls$AdjustedCall == "S") & (Nick_Mahrley_AdjustedCalls$Borderline == "F") ) ), "I","C")
Nick_Mahrley_AdjustedCalls$Call = ifelse( ( (Nick_Mahrley_AdjustedCalls$type == 'S') & ((Nick_Mahrley_AdjustedCalls$AdjustedCall == "B") & (Nick_Mahrley_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Nick_Mahrley_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Nick_Mahrley_Incorrect = subset(Nick_Mahrley_AdjustedCalls, Nick_Mahrley_AdjustedCalls$Call == "I")
print(Nick_Mahrley_Incorrect$player_name)
print(Nick_Mahrley_Incorrect$AdjustedCall)
