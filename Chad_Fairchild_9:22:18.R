# 9/22/18 : Giants vs Cardinals : Chad Fairchild

# 142 pitches were called strikes/balls

# The robot-ump called 47 of those pitches as called strikes & 95 as balls

# Chad Fairchild called 51 of those pitches as called strikes & 91 as balls 

# Accuracy: 90%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Chad_Fairchild <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chad_Fairchild_9:22:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Chad_Fairchild)
names(Chad_Fairchild)
is.na(Chad_Fairchild)
colSums(is.na(Chad_Fairchild)) 
Chad_Fairchild = Chad_Fairchild[,colSums(is.na(Chad_Fairchild)) == 0] 
dim(Chad_Fairchild)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Chad_Fairchild = Chad_Fairchild[ , !(names(Chad_Fairchild) %in% drops)]
dim(Chad_Fairchild)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Chad_Fairchild_train = Chad_Fairchild[0:(0.8 * nrow(Chad_Fairchild)),]
dim(Chad_Fairchild_train)
prop.table(table(Chad_Fairchild_train$type))
Chad_Fairchild_test = Chad_Fairchild[(0.8*nrow(Chad_Fairchild)):nrow(Chad_Fairchild),]
dim(Chad_Fairchild_test)
prop.table(table(Chad_Fairchild_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Chad_Fairchild_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Chad_Fairchild_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Chad_Fairchild_test$type))

# Subset for Borderline Calls
Chad_Fairchild$Borderline = ifelse(((abs(Chad_Fairchild$plate_x)> 0.748) & (abs(Chad_Fairchild$plate_x)<0.914)) 
                                & (((Chad_Fairchild$plate_z > Chad_Fairchild$sz_top-0.83) & (Chad_Fairchild$plate_z < Chad_Fairchild$sz_top+0.83))
                                   | (((Chad_Fairchild$plate_z)<Chad_Fairchild$sz_bot+0.83) & ((Chad_Fairchild$plate_z) > Chad_Fairchild$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Chad_Fairchild_Strikes = subset(Chad_Fairchild, Chad_Fairchild$type == "S")
Chad_Fairchild_Balls = subset(Chad_Fairchild, Chad_Fairchild$type == "B")

# Borderline
Chad_Fairchild_Borderline = subset(Chad_Fairchild, Chad_Fairchild$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Chad_Fairchild_Strikes$AdjustedCall = ifelse((Chad_Fairchild_Strikes$plate_x < 0.833 & Chad_Fairchild_Strikes$plate_x > -0.833) & (Chad_Fairchild_Strikes$plate_z > Chad_Fairchild_Strikes$sz_bot & Chad_Fairchild_Strikes$plate_z < Chad_Fairchild_Strikes$sz_top), 'S', 'B')
table(Chad_Fairchild_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Chad_Fairchild_Balls$AdjustedCall = ifelse((Chad_Fairchild_Balls$plate_x > 0.833 | Chad_Fairchild_Balls$plate_x < -0.833)|(Chad_Fairchild_Balls$plate_z < Chad_Fairchild_Balls$sz_bot | Chad_Fairchild_Balls$plate_z > Chad_Fairchild_Balls$sz_top),'B','S')
table(Chad_Fairchild_Balls$AdjustedCall)

# Borderline
Chad_Fairchild_Borderline$AdjustedCall = ifelse((Chad_Fairchild_Borderline$plate_x < 0.833 & Chad_Fairchild_Borderline$plate_x > -0.833) & (Chad_Fairchild_Borderline$plate_z > Chad_Fairchild_Borderline$sz_bot & Chad_Fairchild_Borderline$plate_z < Chad_Fairchild_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Chad_Fairchild_AdjustedCalls = rbind(Chad_Fairchild_Strikes,Chad_Fairchild_Balls)
Chad_Fairchild_AdjustedCalls$OnFieldRuling = ifelse(Chad_Fairchild_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Chad_Fairchild)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Chad_Fairchild_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Chad_Fairchild_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Chad_Fairchild_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Chad_Fairchild_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Chad_Fairchild_AdjustedCalls$Call = ifelse( ((Chad_Fairchild_AdjustedCalls$type == 'B') & ( (Chad_Fairchild_AdjustedCalls$AdjustedCall == "B") | (Chad_Fairchild_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Chad_Fairchild_AdjustedCalls$Call = ifelse( ((Chad_Fairchild_AdjustedCalls$type == 'S') & ((Chad_Fairchild_AdjustedCalls$AdjustedCall == "S") | (Chad_Fairchild_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Chad_Fairchild_AdjustedCalls$Call = ifelse( ( (Chad_Fairchild_AdjustedCalls$type == 'B') & ((Chad_Fairchild_AdjustedCalls$AdjustedCall == "S") & (Chad_Fairchild_AdjustedCalls$Borderline == "F") ) ), "I","C")
Chad_Fairchild_AdjustedCalls$Call = ifelse( ( (Chad_Fairchild_AdjustedCalls$type == 'S') & ((Chad_Fairchild_AdjustedCalls$AdjustedCall == "B") & (Chad_Fairchild_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Chad_Fairchild_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Chad_Fairchild_Incorrect = subset(Chad_Fairchild_AdjustedCalls, Chad_Fairchild_AdjustedCalls$Call == "I")
print(Chad_Fairchild_Incorrect$player_name)
print(Chad_Fairchild_Incorrect$AdjustedCall)
