# 9/14/18 : Giants vs Rockies : Gabe Morales

#  115 pitches were called strikes/balls

# The robot-ump called 39 of those pitches as called strikes & 76 as balls

# Gabe Morales called 42 of those pitches as called strikes & 73 as balls 

# Accuracy: 89%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Gabe_Morales <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Gabe_Morales_9:14:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Gabe_Morales)
names(Gabe_Morales)
is.na(Gabe_Morales)
colSums(is.na(Gabe_Morales)) 
Gabe_Morales = Gabe_Morales[,colSums(is.na(Gabe_Morales)) == 0] 
dim(Gabe_Morales)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Gabe_Morales = Gabe_Morales[ , !(names(Gabe_Morales) %in% drops)]
dim(Gabe_Morales)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Gabe_Morales_train = Gabe_Morales[0:(0.8 * nrow(Gabe_Morales)),]
dim(Gabe_Morales_train)
prop.table(table(Gabe_Morales_train$type))
Gabe_Morales_test = Gabe_Morales[(0.8*nrow(Gabe_Morales)):nrow(Gabe_Morales),]
dim(Gabe_Morales_test)
prop.table(table(Gabe_Morales_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Gabe_Morales_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Gabe_Morales_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Gabe_Morales_test$type))

# Subset for Borderline Calls
Gabe_Morales$Borderline = ifelse(((abs(Gabe_Morales$plate_x)> 0.748) & (abs(Gabe_Morales$plate_x)<0.914)) 
                                & (((Gabe_Morales$plate_z > Gabe_Morales$sz_top-0.83) & (Gabe_Morales$plate_z < Gabe_Morales$sz_top+0.83))
                                   | (((Gabe_Morales$plate_z)<Gabe_Morales$sz_bot+0.83) & ((Gabe_Morales$plate_z) > Gabe_Morales$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Gabe_Morales_Strikes = subset(Gabe_Morales, Gabe_Morales$type == "S")
Gabe_Morales_Balls = subset(Gabe_Morales, Gabe_Morales$type == "B")

# Borderline
Gabe_Morales_Borderline = subset(Gabe_Morales, Gabe_Morales$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Gabe_Morales_Strikes$AdjustedCall = ifelse((Gabe_Morales_Strikes$plate_x < 0.833 & Gabe_Morales_Strikes$plate_x > -0.833) & (Gabe_Morales_Strikes$plate_z > Gabe_Morales_Strikes$sz_bot & Gabe_Morales_Strikes$plate_z < Gabe_Morales_Strikes$sz_top), 'S', 'B')
table(Gabe_Morales_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Gabe_Morales_Balls$AdjustedCall = ifelse((Gabe_Morales_Balls$plate_x > 0.833 | Gabe_Morales_Balls$plate_x < -0.833)|(Gabe_Morales_Balls$plate_z < Gabe_Morales_Balls$sz_bot | Gabe_Morales_Balls$plate_z > Gabe_Morales_Balls$sz_top),'B','S')
table(Gabe_Morales_Balls$AdjustedCall)

# Borderline
Gabe_Morales_Borderline$AdjustedCall = ifelse((Gabe_Morales_Borderline$plate_x < 0.833 & Gabe_Morales_Borderline$plate_x > -0.833) & (Gabe_Morales_Borderline$plate_z > Gabe_Morales_Borderline$sz_bot & Gabe_Morales_Borderline$plate_z < Gabe_Morales_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Gabe_Morales_AdjustedCalls = rbind(Gabe_Morales_Strikes,Gabe_Morales_Balls)
Gabe_Morales_AdjustedCalls$OnFieldRuling = ifelse(Gabe_Morales_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Gabe_Morales)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Gabe_Morales_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Gabe_Morales_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Gabe_Morales_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Gabe_Morales_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Gabe_Morales_AdjustedCalls$Call = ifelse( ((Gabe_Morales_AdjustedCalls$type == 'B') & ( (Gabe_Morales_AdjustedCalls$AdjustedCall == "B") | (Gabe_Morales_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Gabe_Morales_AdjustedCalls$Call = ifelse( ((Gabe_Morales_AdjustedCalls$type == 'S') & ((Gabe_Morales_AdjustedCalls$AdjustedCall == "S") | (Gabe_Morales_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Gabe_Morales_AdjustedCalls$Call = ifelse( ( (Gabe_Morales_AdjustedCalls$type == 'B') & ((Gabe_Morales_AdjustedCalls$AdjustedCall == "S") & (Gabe_Morales_AdjustedCalls$Borderline == "F") ) ), "I","C")
Gabe_Morales_AdjustedCalls$Call = ifelse( ( (Gabe_Morales_AdjustedCalls$type == 'S') & ((Gabe_Morales_AdjustedCalls$AdjustedCall == "B") & (Gabe_Morales_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Gabe_Morales_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Gabe_Morales_Incorrect = subset(Gabe_Morales_AdjustedCalls, Gabe_Morales_AdjustedCalls$Call == "I")
print(Gabe_Morales_Incorrect$player_name)
print(Gabe_Morales_Incorrect$AdjustedCall)
