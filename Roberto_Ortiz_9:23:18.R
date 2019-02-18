# 9/23/18 : Giants vs Cardinals : Roberto Ortiz

# 132 pitches were called strikes/balls

# The robot-ump called 42 of those pitches as called strikes & 90 as balls

# Roberto Ortiz called 42 of those pitches as called strikes & 90 as balls 

# Accuracy: 88%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Roberto_Ortiz <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Roberto_Ortiz_9:23:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Roberto_Ortiz)
names(Roberto_Ortiz)
is.na(Roberto_Ortiz)
colSums(is.na(Roberto_Ortiz)) 
Roberto_Ortiz = Roberto_Ortiz[,colSums(is.na(Roberto_Ortiz)) == 0] 
dim(Roberto_Ortiz)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Roberto_Ortiz = Roberto_Ortiz[ , !(names(Roberto_Ortiz) %in% drops)]
dim(Roberto_Ortiz)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Roberto_Ortiz_train = Roberto_Ortiz[0:(0.8 * nrow(Roberto_Ortiz)),]
dim(Roberto_Ortiz_train)
prop.table(table(Roberto_Ortiz_train$type))
Roberto_Ortiz_test = Roberto_Ortiz[(0.8*nrow(Roberto_Ortiz)):nrow(Roberto_Ortiz),]
dim(Roberto_Ortiz_test)
prop.table(table(Roberto_Ortiz_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Roberto_Ortiz_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Roberto_Ortiz_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Roberto_Ortiz_test$type))

# Subset for Borderline Calls
Roberto_Ortiz$Borderline = ifelse(((abs(Roberto_Ortiz$plate_x)> 0.748) & (abs(Roberto_Ortiz$plate_x)<0.914)) 
                                & (((Roberto_Ortiz$plate_z > Roberto_Ortiz$sz_top-0.83) & (Roberto_Ortiz$plate_z < Roberto_Ortiz$sz_top+0.83))
                                   | (((Roberto_Ortiz$plate_z)<Roberto_Ortiz$sz_bot+0.83) & ((Roberto_Ortiz$plate_z) > Roberto_Ortiz$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Roberto_Ortiz_Strikes = subset(Roberto_Ortiz, Roberto_Ortiz$type == "S")
Roberto_Ortiz_Balls = subset(Roberto_Ortiz, Roberto_Ortiz$type == "B")

# Borderline
Roberto_Ortiz_Borderline = subset(Roberto_Ortiz, Roberto_Ortiz$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Roberto_Ortiz_Strikes$AdjustedCall = ifelse((Roberto_Ortiz_Strikes$plate_x < 0.833 & Roberto_Ortiz_Strikes$plate_x > -0.833) & (Roberto_Ortiz_Strikes$plate_z > Roberto_Ortiz_Strikes$sz_bot & Roberto_Ortiz_Strikes$plate_z < Roberto_Ortiz_Strikes$sz_top), 'S', 'B')
table(Roberto_Ortiz_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Roberto_Ortiz_Balls$AdjustedCall = ifelse((Roberto_Ortiz_Balls$plate_x > 0.833 | Roberto_Ortiz_Balls$plate_x < -0.833)|(Roberto_Ortiz_Balls$plate_z < Roberto_Ortiz_Balls$sz_bot | Roberto_Ortiz_Balls$plate_z > Roberto_Ortiz_Balls$sz_top),'B','S')
table(Roberto_Ortiz_Balls$AdjustedCall)

# Borderline
Roberto_Ortiz_Borderline$AdjustedCall = ifelse((Roberto_Ortiz_Borderline$plate_x < 0.833 & Roberto_Ortiz_Borderline$plate_x > -0.833) & (Roberto_Ortiz_Borderline$plate_z > Roberto_Ortiz_Borderline$sz_bot & Roberto_Ortiz_Borderline$plate_z < Roberto_Ortiz_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Roberto_Ortiz_AdjustedCalls = rbind(Roberto_Ortiz_Strikes,Roberto_Ortiz_Balls)
Roberto_Ortiz_AdjustedCalls$OnFieldRuling = ifelse(Roberto_Ortiz_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Roberto_Ortiz)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Roberto_Ortiz_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Roberto_Ortiz_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Roberto_Ortiz_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Roberto_Ortiz_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Roberto_Ortiz_AdjustedCalls$Call = ifelse( ((Roberto_Ortiz_AdjustedCalls$type == 'B') & ( (Roberto_Ortiz_AdjustedCalls$AdjustedCall == "B") | (Roberto_Ortiz_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Roberto_Ortiz_AdjustedCalls$Call = ifelse( ((Roberto_Ortiz_AdjustedCalls$type == 'S') & ((Roberto_Ortiz_AdjustedCalls$AdjustedCall == "S") | (Roberto_Ortiz_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Roberto_Ortiz_AdjustedCalls$Call = ifelse( ( (Roberto_Ortiz_AdjustedCalls$type == 'B') & ((Roberto_Ortiz_AdjustedCalls$AdjustedCall == "S") & (Roberto_Ortiz_AdjustedCalls$Borderline == "F") ) ), "I","C")
Roberto_Ortiz_AdjustedCalls$Call = ifelse( ( (Roberto_Ortiz_AdjustedCalls$type == 'S') & ((Roberto_Ortiz_AdjustedCalls$AdjustedCall == "B") & (Roberto_Ortiz_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Roberto_Ortiz_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Roberto_Ortiz_Incorrect = subset(Roberto_Ortiz_AdjustedCalls, Roberto_Ortiz_AdjustedCalls$Call == "I")
print(Roberto_Ortiz_Incorrect$player_name)
print(Roberto_Ortiz_Incorrect$AdjustedCall)
