# 7/8/18 : Giants vs Cardinals : Chris Conroy

# 137 pitches were called strikes/balls

# The robot-ump called 47 of those pitches as called strikes & 90 as balls

# Chris Conroy called 46 of those pitches as called strikes & 91 as balls 

# Accuracy: 95%

Chris_Conroy <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chris_Conroy_7:7:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Chris_Conroy)
names(Chris_Conroy)
is.na(Chris_Conroy)
colSums(is.na(Chris_Conroy)) 
Chris_Conroy = Chris_Conroy[,colSums(is.na(Chris_Conroy)) == 0] 
dim(Chris_Conroy)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Chris_Conroy = Chris_Conroy[ , !(names(Chris_Conroy) %in% drops)]
dim(Chris_Conroy)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Chris_Conroy_train = Chris_Conroy[0:(0.8 * nrow(Chris_Conroy)),]
dim(Chris_Conroy_train)
prop.table(table(Chris_Conroy_train$type))
Chris_Conroy_test = Chris_Conroy[(0.8*nrow(Chris_Conroy)):nrow(Chris_Conroy),]
dim(Chris_Conroy_test)
prop.table(table(Chris_Conroy_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Chris_Conroy_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Chris_Conroy_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Chris_Conroy_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Chris_Conroy_Strikes = subset(Chris_Conroy, Chris_Conroy$type == "S")
Chris_Conroy_Balls = subset(Chris_Conroy, Chris_Conroy$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Chris_Conroy_Strikes$AdjustedCall = ifelse((Chris_Conroy_Strikes$plate_x < 0.833 & Chris_Conroy_Strikes$plate_x > -0.833) & (Chris_Conroy_Strikes$plate_z > Chris_Conroy_Strikes$sz_bot & Chris_Conroy_Strikes$plate_z < Chris_Conroy_Strikes$sz_top), 'S', 'B')
table(Chris_Conroy_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Chris_Conroy_Balls$AdjustedCall = ifelse((Chris_Conroy_Balls$plate_x > 0.833 | Chris_Conroy_Balls$plate_x < -0.833)|(Chris_Conroy_Balls$plate_z < Chris_Conroy_Balls$sz_bot | Chris_Conroy_Balls$plate_z > Chris_Conroy_Balls$sz_top),'B','S')
table(Chris_Conroy_Balls$AdjustedCall)

# Merge to create new dataset
Chris_Conroy_AdjustedCalls = rbind(Chris_Conroy_Strikes,Chris_Conroy_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Chris_Conroy)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Chris_Conroy_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Chris_Conroy_AdjustedCalls$AdjustedCall))







