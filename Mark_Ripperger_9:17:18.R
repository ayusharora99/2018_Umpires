# 9/17/18 : Giants vs Padres : Mark Ripperger

# __NUMBER OF__ pitches were called strikes/balls

# The robot-ump called 38 of those pitches as called strikes & 82 as balls

# Mark Ripperger called 42 of those pitches as called strikes & 79 as balls 

# Accuracy: 92%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Mark_Ripperger <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mark_Ripperger_9:17:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Mark_Ripperger)
names(Mark_Ripperger)
is.na(Mark_Ripperger)
colSums(is.na(Mark_Ripperger)) 
Mark_Ripperger = Mark_Ripperger[,colSums(is.na(Mark_Ripperger)) == 0] 
dim(Mark_Ripperger)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mark_Ripperger = Mark_Ripperger[ , !(names(Mark_Ripperger) %in% drops)]
dim(Mark_Ripperger)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Mark_Ripperger_train = Mark_Ripperger[0:(0.8 * nrow(Mark_Ripperger)),]
dim(Mark_Ripperger_train)
prop.table(table(Mark_Ripperger_train$type))
Mark_Ripperger_test = Mark_Ripperger[(0.8*nrow(Mark_Ripperger)):nrow(Mark_Ripperger),]
dim(Mark_Ripperger_test)
prop.table(table(Mark_Ripperger_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mark_Ripperger_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Mark_Ripperger_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Mark_Ripperger_test$type))

# Subset for Borderline Calls
Mark_Ripperger$Borderline = ifelse(((abs(Mark_Ripperger$plate_x)> 0.748) & (abs(Mark_Ripperger$plate_x)<0.914)) 
                                & (((Mark_Ripperger$plate_z > Mark_Ripperger$sz_top-0.83) & (Mark_Ripperger$plate_z < Mark_Ripperger$sz_top+0.83))
                                   | (((Mark_Ripperger$plate_z)<Mark_Ripperger$sz_bot+0.83) & ((Mark_Ripperger$plate_z) > Mark_Ripperger$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Mark_Ripperger_Strikes = subset(Mark_Ripperger, Mark_Ripperger$type == "S")
Mark_Ripperger_Balls = subset(Mark_Ripperger, Mark_Ripperger$type == "B")

# Borderline
Mark_Ripperger_Borderline = subset(Mark_Ripperger, Mark_Ripperger$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mark_Ripperger_Strikes$AdjustedCall = ifelse((Mark_Ripperger_Strikes$plate_x < 0.833 & Mark_Ripperger_Strikes$plate_x > -0.833) & (Mark_Ripperger_Strikes$plate_z > Mark_Ripperger_Strikes$sz_bot & Mark_Ripperger_Strikes$plate_z < Mark_Ripperger_Strikes$sz_top), 'S', 'B')
table(Mark_Ripperger_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mark_Ripperger_Balls$AdjustedCall = ifelse((Mark_Ripperger_Balls$plate_x > 0.833 | Mark_Ripperger_Balls$plate_x < -0.833)|(Mark_Ripperger_Balls$plate_z < Mark_Ripperger_Balls$sz_bot | Mark_Ripperger_Balls$plate_z > Mark_Ripperger_Balls$sz_top),'B','S')
table(Mark_Ripperger_Balls$AdjustedCall)

# Borderline
Mark_Ripperger_Borderline$AdjustedCall = ifelse((Mark_Ripperger_Borderline$plate_x < 0.833 & Mark_Ripperger_Borderline$plate_x > -0.833) & (Mark_Ripperger_Borderline$plate_z > Mark_Ripperger_Borderline$sz_bot & Mark_Ripperger_Borderline$plate_z < Mark_Ripperger_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Mark_Ripperger_AdjustedCalls = rbind(Mark_Ripperger_Strikes,Mark_Ripperger_Balls)
Mark_Ripperger_AdjustedCalls$OnFieldRuling = ifelse(Mark_Ripperger_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Mark_Ripperger)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Mark_Ripperger_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Mark_Ripperger_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Mark_Ripperger_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Mark_Ripperger_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Mark_Ripperger_AdjustedCalls$Call = ifelse( ((Mark_Ripperger_AdjustedCalls$type == 'B') & ( (Mark_Ripperger_AdjustedCalls$AdjustedCall == "B") | (Mark_Ripperger_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Mark_Ripperger_AdjustedCalls$Call = ifelse( ((Mark_Ripperger_AdjustedCalls$type == 'S') & ((Mark_Ripperger_AdjustedCalls$AdjustedCall == "S") | (Mark_Ripperger_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Mark_Ripperger_AdjustedCalls$Call = ifelse( ( (Mark_Ripperger_AdjustedCalls$type == 'B') & ((Mark_Ripperger_AdjustedCalls$AdjustedCall == "S") & (Mark_Ripperger_AdjustedCalls$Borderline == "F") ) ), "I","C")
Mark_Ripperger_AdjustedCalls$Call = ifelse( ( (Mark_Ripperger_AdjustedCalls$type == 'S') & ((Mark_Ripperger_AdjustedCalls$AdjustedCall == "B") & (Mark_Ripperger_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Mark_Ripperger_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Mark_Ripperger_Incorrect = subset(Mark_Ripperger_AdjustedCalls, Mark_Ripperger_AdjustedCalls$Call == "I")
print(Mark_Ripperger_Incorrect$player_name)
print(Mark_Ripperger_Incorrect$AdjustedCall)
