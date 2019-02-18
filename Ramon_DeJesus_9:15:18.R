# 9/15/18 : Giants vs Rockies : Ramon DeJesus

#  109 pitches were called strikes/balls

# The robot-ump called 36 of those pitches as called strikes & 73 as balls

# Ramon DeJesus called 36 of those pitches as called strikes & 73 as balls 

# Accuracy: 93%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Ramon_DeJesus <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Ramon_DeJesus_9:15:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Ramon_DeJesus)
names(Ramon_DeJesus)
is.na(Ramon_DeJesus)
colSums(is.na(Ramon_DeJesus)) 
Ramon_DeJesus = Ramon_DeJesus[,colSums(is.na(Ramon_DeJesus)) == 0] 
dim(Ramon_DeJesus)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Ramon_DeJesus = Ramon_DeJesus[ , !(names(Ramon_DeJesus) %in% drops)]
dim(Ramon_DeJesus)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Ramon_DeJesus_train = Ramon_DeJesus[0:(0.8 * nrow(Ramon_DeJesus)),]
dim(Ramon_DeJesus_train)
prop.table(table(Ramon_DeJesus_train$type))
Ramon_DeJesus_test = Ramon_DeJesus[(0.8*nrow(Ramon_DeJesus)):nrow(Ramon_DeJesus),]
dim(Ramon_DeJesus_test)
prop.table(table(Ramon_DeJesus_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Ramon_DeJesus_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Ramon_DeJesus_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Ramon_DeJesus_test$type))

# Subset for Borderline Calls
Ramon_DeJesus$Borderline = ifelse(((abs(Ramon_DeJesus$plate_x)> 0.748) & (abs(Ramon_DeJesus$plate_x)<0.914)) 
                                & (((Ramon_DeJesus$plate_z > Ramon_DeJesus$sz_top-0.83) & (Ramon_DeJesus$plate_z < Ramon_DeJesus$sz_top+0.83))
                                   | (((Ramon_DeJesus$plate_z)<Ramon_DeJesus$sz_bot+0.83) & ((Ramon_DeJesus$plate_z) > Ramon_DeJesus$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Ramon_DeJesus_Strikes = subset(Ramon_DeJesus, Ramon_DeJesus$type == "S")
Ramon_DeJesus_Balls = subset(Ramon_DeJesus, Ramon_DeJesus$type == "B")

# Borderline
Ramon_DeJesus_Borderline = subset(Ramon_DeJesus, Ramon_DeJesus$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Ramon_DeJesus_Strikes$AdjustedCall = ifelse((Ramon_DeJesus_Strikes$plate_x < 0.833 & Ramon_DeJesus_Strikes$plate_x > -0.833) & (Ramon_DeJesus_Strikes$plate_z > Ramon_DeJesus_Strikes$sz_bot & Ramon_DeJesus_Strikes$plate_z < Ramon_DeJesus_Strikes$sz_top), 'S', 'B')
table(Ramon_DeJesus_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Ramon_DeJesus_Balls$AdjustedCall = ifelse((Ramon_DeJesus_Balls$plate_x > 0.833 | Ramon_DeJesus_Balls$plate_x < -0.833)|(Ramon_DeJesus_Balls$plate_z < Ramon_DeJesus_Balls$sz_bot | Ramon_DeJesus_Balls$plate_z > Ramon_DeJesus_Balls$sz_top),'B','S')
table(Ramon_DeJesus_Balls$AdjustedCall)

# Borderline
Ramon_DeJesus_Borderline$AdjustedCall = ifelse((Ramon_DeJesus_Borderline$plate_x < 0.833 & Ramon_DeJesus_Borderline$plate_x > -0.833) & (Ramon_DeJesus_Borderline$plate_z > Ramon_DeJesus_Borderline$sz_bot & Ramon_DeJesus_Borderline$plate_z < Ramon_DeJesus_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Ramon_DeJesus_AdjustedCalls = rbind(Ramon_DeJesus_Strikes,Ramon_DeJesus_Balls)
Ramon_DeJesus_AdjustedCalls$OnFieldRuling = ifelse(Ramon_DeJesus_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Ramon_DeJesus)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Ramon_DeJesus_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Ramon_DeJesus_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Ramon_DeJesus_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Ramon_DeJesus_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Ramon_DeJesus_AdjustedCalls$Call = ifelse( ((Ramon_DeJesus_AdjustedCalls$type == 'B') & ( (Ramon_DeJesus_AdjustedCalls$AdjustedCall == "B") | (Ramon_DeJesus_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Ramon_DeJesus_AdjustedCalls$Call = ifelse( ((Ramon_DeJesus_AdjustedCalls$type == 'S') & ((Ramon_DeJesus_AdjustedCalls$AdjustedCall == "S") | (Ramon_DeJesus_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Ramon_DeJesus_AdjustedCalls$Call = ifelse( ( (Ramon_DeJesus_AdjustedCalls$type == 'B') & ((Ramon_DeJesus_AdjustedCalls$AdjustedCall == "S") & (Ramon_DeJesus_AdjustedCalls$Borderline == "F") ) ), "I","C")
Ramon_DeJesus_AdjustedCalls$Call = ifelse( ( (Ramon_DeJesus_AdjustedCalls$type == 'S') & ((Ramon_DeJesus_AdjustedCalls$AdjustedCall == "B") & (Ramon_DeJesus_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Ramon_DeJesus_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Ramon_DeJesus_Incorrect = subset(Ramon_DeJesus_AdjustedCalls, Ramon_DeJesus_AdjustedCalls$Call == "I")
print(Ramon_DeJesus_Incorrect$player_name)
print(Ramon_DeJesus_Incorrect$AdjustedCall)
