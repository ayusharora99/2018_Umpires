# 9/12/18 : Giants vs Braves : John Tumpane

# 151 pitches were called strikes/balls

# The robot-ump called 52 of those pitches as called strikes & 99 as balls

# John Tumpane called 49 of those pitches as called strikes & 112 as balls 

# Accuracy: 87%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
John_Tumpane <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/John_Tumpane_9:12:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(John_Tumpane)
names(John_Tumpane)
is.na(John_Tumpane)
colSums(is.na(John_Tumpane)) 
John_Tumpane = John_Tumpane[,colSums(is.na(John_Tumpane)) == 0] 
dim(John_Tumpane)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
John_Tumpane = John_Tumpane[ , !(names(John_Tumpane) %in% drops)]
dim(John_Tumpane)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
John_Tumpane_train = John_Tumpane[0:(0.8 * nrow(John_Tumpane)),]
dim(John_Tumpane_train)
prop.table(table(John_Tumpane_train$type))
John_Tumpane_test = John_Tumpane[(0.8*nrow(John_Tumpane)):nrow(John_Tumpane),]
dim(John_Tumpane_test)
prop.table(table(John_Tumpane_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = John_Tumpane_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = John_Tumpane_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, John_Tumpane_test$type))

# Subset for Borderline Calls
John_Tumpane$Borderline = ifelse(((abs(John_Tumpane$plate_x)> 0.748) & (abs(John_Tumpane$plate_x)<0.914)) 
                                & (((John_Tumpane$plate_z > John_Tumpane$sz_top-0.83) & (John_Tumpane$plate_z < John_Tumpane$sz_top+0.83))
                                   | (((John_Tumpane$plate_z)<John_Tumpane$sz_bot+0.83) & ((John_Tumpane$plate_z) > John_Tumpane$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
John_Tumpane_Strikes = subset(John_Tumpane, John_Tumpane$type == "S")
John_Tumpane_Balls = subset(John_Tumpane, John_Tumpane$type == "B")

# Borderline
John_Tumpane_Borderline = subset(John_Tumpane, John_Tumpane$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
John_Tumpane_Strikes$AdjustedCall = ifelse((John_Tumpane_Strikes$plate_x < 0.833 & John_Tumpane_Strikes$plate_x > -0.833) & (John_Tumpane_Strikes$plate_z > John_Tumpane_Strikes$sz_bot & John_Tumpane_Strikes$plate_z < John_Tumpane_Strikes$sz_top), 'S', 'B')
table(John_Tumpane_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
John_Tumpane_Balls$AdjustedCall = ifelse((John_Tumpane_Balls$plate_x > 0.833 | John_Tumpane_Balls$plate_x < -0.833)|(John_Tumpane_Balls$plate_z < John_Tumpane_Balls$sz_bot | John_Tumpane_Balls$plate_z > John_Tumpane_Balls$sz_top),'B','S')
table(John_Tumpane_Balls$AdjustedCall)

# Borderline
John_Tumpane_Borderline$AdjustedCall = ifelse((John_Tumpane_Borderline$plate_x < 0.833 & John_Tumpane_Borderline$plate_x > -0.833) & (John_Tumpane_Borderline$plate_z > John_Tumpane_Borderline$sz_bot & John_Tumpane_Borderline$plate_z < John_Tumpane_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
John_Tumpane_AdjustedCalls = rbind(John_Tumpane_Strikes,John_Tumpane_Balls)
John_Tumpane_AdjustedCalls$OnFieldRuling = ifelse(John_Tumpane_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = John_Tumpane)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = John_Tumpane_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,John_Tumpane_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = John_Tumpane_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,John_Tumpane_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
John_Tumpane_AdjustedCalls$Call = ifelse( ((John_Tumpane_AdjustedCalls$type == 'B') & ( (John_Tumpane_AdjustedCalls$AdjustedCall == "B") | (John_Tumpane_AdjustedCalls$Borderline == "T") ) ), "C","I" )
John_Tumpane_AdjustedCalls$Call = ifelse( ((John_Tumpane_AdjustedCalls$type == 'S') & ((John_Tumpane_AdjustedCalls$AdjustedCall == "S") | (John_Tumpane_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
John_Tumpane_AdjustedCalls$Call = ifelse( ( (John_Tumpane_AdjustedCalls$type == 'B') & ((John_Tumpane_AdjustedCalls$AdjustedCall == "S") & (John_Tumpane_AdjustedCalls$Borderline == "F") ) ), "I","C")
John_Tumpane_AdjustedCalls$Call = ifelse( ( (John_Tumpane_AdjustedCalls$type == 'S') & ((John_Tumpane_AdjustedCalls$AdjustedCall == "B") & (John_Tumpane_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(John_Tumpane_AdjustedCalls$Call)


# Which Pitchers Recieved the InCorrect Calls
John_Tumpane_Incorrect = subset(John_Tumpane_AdjustedCalls, John_Tumpane_AdjustedCalls$Call == "I")
print(John_Tumpane_Incorrect$player_name)
print(John_Tumpane_Incorrect$AdjustedCall)
