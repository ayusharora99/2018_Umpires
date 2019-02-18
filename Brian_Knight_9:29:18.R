# 9/29/18 : Giants vs Dodgers : Brian Knight

# 146 pitches were called strikes/balls

# The robot-ump called 38 of those pitches as called strikes & 108 as balls

# Brian Knight called 53 of those pitches as called strikes & 38 as balls 

# Accuracy: 87%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Brian_Knight <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Brian_Knight_9:28:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Brian_Knight)
names(Brian_Knight)
is.na(Brian_Knight)
colSums(is.na(Brian_Knight)) 
Brian_Knight = Brian_Knight[,colSums(is.na(Brian_Knight)) == 0] 
dim(Brian_Knight)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Brian_Knight = Brian_Knight[ , !(names(Brian_Knight) %in% drops)]
dim(Brian_Knight)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Brian_Knight_train = Brian_Knight[0:(0.8 * nrow(Brian_Knight)),]
dim(Brian_Knight_train)
prop.table(table(Brian_Knight_train$type))
Brian_Knight_test = Brian_Knight[(0.8*nrow(Brian_Knight)):nrow(Brian_Knight),]
dim(Brian_Knight_test)
prop.table(table(Brian_Knight_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Brian_Knight_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Brian_Knight_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Brian_Knight_test$type))

# Subset for Borderline Calls
Brian_Knight$Borderline = ifelse(((abs(Brian_Knight$plate_x)> 0.748) & (abs(Brian_Knight$plate_x)<0.914)) 
                                & (((Brian_Knight$plate_z > Brian_Knight$sz_top-0.83) & (Brian_Knight$plate_z < Brian_Knight$sz_top+0.83))
                                   | (((Brian_Knight$plate_z)<Brian_Knight$sz_bot+0.83) & ((Brian_Knight$plate_z) > Brian_Knight$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Brian_Knight_Strikes = subset(Brian_Knight, Brian_Knight$type == "S")
Brian_Knight_Balls = subset(Brian_Knight, Brian_Knight$type == "B")

# Borderline
Brian_Knight_Borderline = subset(Brian_Knight, Brian_Knight$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Brian_Knight_Strikes$AdjustedCall = ifelse((Brian_Knight_Strikes$plate_x < 0.833 & Brian_Knight_Strikes$plate_x > -0.833) & (Brian_Knight_Strikes$plate_z > Brian_Knight_Strikes$sz_bot & Brian_Knight_Strikes$plate_z < Brian_Knight_Strikes$sz_top), 'S', 'B')
table(Brian_Knight_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Brian_Knight_Balls$AdjustedCall = ifelse((Brian_Knight_Balls$plate_x > 0.833 | Brian_Knight_Balls$plate_x < -0.833)|(Brian_Knight_Balls$plate_z < Brian_Knight_Balls$sz_bot | Brian_Knight_Balls$plate_z > Brian_Knight_Balls$sz_top),'B','S')
table(Brian_Knight_Balls$AdjustedCall)

# Borderline
Brian_Knight_Borderline$AdjustedCall = ifelse((Brian_Knight_Borderline$plate_x < 0.833 & Brian_Knight_Borderline$plate_x > -0.833) & (Brian_Knight_Borderline$plate_z > Brian_Knight_Borderline$sz_bot & Brian_Knight_Borderline$plate_z < Brian_Knight_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Brian_Knight_AdjustedCalls = rbind(Brian_Knight_Strikes,Brian_Knight_Balls)
Brian_Knight_AdjustedCalls$OnFieldRuling = ifelse(Brian_Knight_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Brian_Knight)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Brian_Knight_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Brian_Knight_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Brian_Knight_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Brian_Knight_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Brian_Knight_AdjustedCalls$Call = ifelse( ((Brian_Knight_AdjustedCalls$type == 'B') & ( (Brian_Knight_AdjustedCalls$AdjustedCall == "B") | (Brian_Knight_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Brian_Knight_AdjustedCalls$Call = ifelse( ((Brian_Knight_AdjustedCalls$type == 'S') & ((Brian_Knight_AdjustedCalls$AdjustedCall == "S") | (Brian_Knight_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Brian_Knight_AdjustedCalls$Call = ifelse( ( (Brian_Knight_AdjustedCalls$type == 'B') & ((Brian_Knight_AdjustedCalls$AdjustedCall == "S") & (Brian_Knight_AdjustedCalls$Borderline == "F") ) ), "I","C")
Brian_Knight_AdjustedCalls$Call = ifelse( ( (Brian_Knight_AdjustedCalls$type == 'S') & ((Brian_Knight_AdjustedCalls$AdjustedCall == "B") & (Brian_Knight_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Brian_Knight_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Brian_Knight_Incorrect = subset(Brian_Knight_AdjustedCalls, Brian_Knight_AdjustedCalls$Call == "I")
print(Brian_Knight_Incorrect$player_name)
print(Brian_Knight_Incorrect$AdjustedCall)
