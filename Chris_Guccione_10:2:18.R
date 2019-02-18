# 10/2/18 : Rockies vs Cubs : Chris Guccione

# 192 pitches were called strikes/balls

# The robot-ump called 60 of those pitches as called strikes & 132 as balls

# Chris Guccione called 77 of those pitches as called strikes & 115 as balls 

# Accuracy: 91%
# Borderline Pitch Accuracy: 84%
# 178 Correct vs 14 Incorrect (10 against Cubs, 4 against Rockies)

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Chris_Guccione <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chris_Guccione_10:2:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Chris_Guccione)
names(Chris_Guccione)
is.na(Chris_Guccione)
colSums(is.na(Chris_Guccione)) 
Chris_Guccione = Chris_Guccione[,colSums(is.na(Chris_Guccione)) == 0] 
dim(Chris_Guccione)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Chris_Guccione = Chris_Guccione[ , !(names(Chris_Guccione) %in% drops)]
dim(Chris_Guccione)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Chris_Guccione_train = Chris_Guccione[0:(0.8 * nrow(Chris_Guccione)),]
dim(Chris_Guccione_train)
prop.table(table(Chris_Guccione_train$type))
Chris_Guccione_test = Chris_Guccione[(0.8*nrow(Chris_Guccione)):nrow(Chris_Guccione),]
dim(Chris_Guccione_test)
prop.table(table(Chris_Guccione_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Chris_Guccione_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Chris_Guccione_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Chris_Guccione_test$type))

# Subset for Borderline Calls
Chris_Guccione$Borderline = ifelse(((abs(Chris_Guccione$plate_x)> 0.748) & (abs(Chris_Guccione$plate_x)<0.914)) 
                                & (((Chris_Guccione$plate_z > Chris_Guccione$sz_top-0.83) & (Chris_Guccione$plate_z < Chris_Guccione$sz_top+0.83))
                                   | (((Chris_Guccione$plate_z)<Chris_Guccione$sz_bot+0.83) & ((Chris_Guccione$plate_z) > Chris_Guccione$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Chris_Guccione_Strikes = subset(Chris_Guccione, Chris_Guccione$type == "S")
Chris_Guccione_Balls = subset(Chris_Guccione, Chris_Guccione$type == "B")

# Borderline
Chris_Guccione_Borderline = subset(Chris_Guccione, Chris_Guccione$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Chris_Guccione_Strikes$AdjustedCall = ifelse((Chris_Guccione_Strikes$plate_x < 0.833 & Chris_Guccione_Strikes$plate_x > -0.833) & (Chris_Guccione_Strikes$plate_z > Chris_Guccione_Strikes$sz_bot & Chris_Guccione_Strikes$plate_z < Chris_Guccione_Strikes$sz_top), 'S', 'B')
table(Chris_Guccione_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Chris_Guccione_Balls$AdjustedCall = ifelse((Chris_Guccione_Balls$plate_x > 0.833 | Chris_Guccione_Balls$plate_x < -0.833)|(Chris_Guccione_Balls$plate_z < Chris_Guccione_Balls$sz_bot | Chris_Guccione_Balls$plate_z > Chris_Guccione_Balls$sz_top),'B','S')
table(Chris_Guccione_Balls$AdjustedCall)

# Borderline
Chris_Guccione_Borderline$AdjustedCall = ifelse((Chris_Guccione_Borderline$plate_x < 0.833 & Chris_Guccione_Borderline$plate_x > -0.833) & (Chris_Guccione_Borderline$plate_z > Chris_Guccione_Borderline$sz_bot & Chris_Guccione_Borderline$plate_z < Chris_Guccione_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Chris_Guccione_AdjustedCalls = rbind(Chris_Guccione_Strikes,Chris_Guccione_Balls)
Chris_Guccione_AdjustedCalls$OnFieldRuling = ifelse(Chris_Guccione_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Chris_Guccione)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Chris_Guccione_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Chris_Guccione_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Chris_Guccione_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Chris_Guccione_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Chris_Guccione_AdjustedCalls$Call = ifelse( ((Chris_Guccione_AdjustedCalls$type == 'B') & ( (Chris_Guccione_AdjustedCalls$AdjustedCall == "B") | (Chris_Guccione_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect" )
Chris_Guccione_AdjustedCalls$Call = ifelse( ((Chris_Guccione_AdjustedCalls$type == 'S') & ((Chris_Guccione_AdjustedCalls$AdjustedCall == "S") | (Chris_Guccione_AdjustedCalls$Borderline == "T") ) ), "Correct","Incorrect")

# InCorrect Calls
Chris_Guccione_AdjustedCalls$Call = ifelse( ( (Chris_Guccione_AdjustedCalls$type == 'B') & ((Chris_Guccione_AdjustedCalls$AdjustedCall == "S") & (Chris_Guccione_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")
Chris_Guccione_AdjustedCalls$Call = ifelse( ( (Chris_Guccione_AdjustedCalls$type == 'S') & ((Chris_Guccione_AdjustedCalls$AdjustedCall == "B") & (Chris_Guccione_AdjustedCalls$Borderline == "F") ) ), "Incorrect","Correct")

table(Chris_Guccione_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Chris_Guccione_Incorrect = subset(Chris_Guccione_AdjustedCalls, Chris_Guccione_AdjustedCalls$Call == "I")
View(Chris_Guccione_Incorrect$player_name)
print(Chris_Guccione_Incorrect$player_name)
print(Chris_Guccione_Incorrect$AdjustedCall)
