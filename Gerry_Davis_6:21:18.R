# 6/21/18 : Giants vs __OPPONENT__ : Gerry Davis

# 136 pitches were called strikes/balls

# The robot-ump called 41 of those pitches as called strikes & 95 as balls

# Gerry Davis called 55 of those pitches as called strikes & 81 as balls 

# Accuracy: 88%

Gerry_Davis <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Gerry_Davis_6:21:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Gerry_Davis)
names(Gerry_Davis)
is.na(Gerry_Davis)
colSums(is.na(Gerry_Davis)) 
Gerry_Davis = Gerry_Davis[,colSums(is.na(Gerry_Davis)) == 0] 
dim(Gerry_Davis)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Gerry_Davis = Gerry_Davis[ , !(names(Gerry_Davis) %in% drops)]
dim(Gerry_Davis)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Gerry_Davis_train = Gerry_Davis[0:(0.8 * nrow(Gerry_Davis)),]
dim(Gerry_Davis_train)
prop.table(table(Gerry_Davis_train$type))
Gerry_Davis_test = Gerry_Davis[(0.8*nrow(Gerry_Davis)):nrow(Gerry_Davis),]
dim(Gerry_Davis_test)
prop.table(table(Gerry_Davis_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Gerry_Davis_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Gerry_Davis_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Gerry_Davis_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Gerry_Davis_Strikes = subset(Gerry_Davis, Gerry_Davis$type == "S")
Gerry_Davis_Balls = subset(Gerry_Davis, Gerry_Davis$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Gerry_Davis_Strikes$AdjustedCall = ifelse((Gerry_Davis_Strikes$plate_x < 0.833 & Gerry_Davis_Strikes$plate_x > -0.833) & (Gerry_Davis_Strikes$plate_z > Gerry_Davis_Strikes$sz_bot & Gerry_Davis_Strikes$plate_z < Gerry_Davis_Strikes$sz_top), 'S', 'B')
table(Gerry_Davis_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Gerry_Davis_Balls$AdjustedCall = ifelse((Gerry_Davis_Balls$plate_x > 0.833 | Gerry_Davis_Balls$plate_x < -0.833)|(Gerry_Davis_Balls$plate_z < Gerry_Davis_Balls$sz_bot | Gerry_Davis_Balls$plate_z > Gerry_Davis_Balls$sz_top),'B','S')
table(Gerry_Davis_Balls$AdjustedCall)

# Merge to create new dataset
Gerry_Davis_AdjustedCalls = rbind(Gerry_Davis_Strikes,Gerry_Davis_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Gerry_Davis)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Gerry_Davis_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Gerry_Davis_AdjustedCalls$AdjustedCall))







