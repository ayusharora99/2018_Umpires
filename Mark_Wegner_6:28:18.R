# 6/28/18 : Giants vs __OPPONENT__ : Mark Wegner

# 122 pitches were called strikes/balls

# The robot-ump called 31 of those pitches as called strikes & 99 as balls

# Mark Wegner called 35 of those pitches as called strikes & 95 as balls 

# Accuracy: 95%

Mark_Wegner <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mark_Wegner_6:28:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Mark_Wegner)
names(Mark_Wegner)
is.na(Mark_Wegner)
colSums(is.na(Mark_Wegner)) 
Mark_Wegner = Mark_Wegner[,colSums(is.na(Mark_Wegner)) == 0] 
dim(Mark_Wegner)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mark_Wegner = Mark_Wegner[ , !(names(Mark_Wegner) %in% drops)]
dim(Mark_Wegner)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Mark_Wegner_train = Mark_Wegner[0:(0.8 * nrow(Mark_Wegner)),]
dim(Mark_Wegner_train)
prop.table(table(Mark_Wegner_train$type))
Mark_Wegner_test = Mark_Wegner[(0.8*nrow(Mark_Wegner)):nrow(Mark_Wegner),]
dim(Mark_Wegner_test)
prop.table(table(Mark_Wegner_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mark_Wegner_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Mark_Wegner_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Mark_Wegner_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Mark_Wegner_Strikes = subset(Mark_Wegner, Mark_Wegner$type == "S")
Mark_Wegner_Balls = subset(Mark_Wegner, Mark_Wegner$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mark_Wegner_Strikes$AdjustedCall = ifelse((Mark_Wegner_Strikes$plate_x < 0.833 & Mark_Wegner_Strikes$plate_x > -0.833) & (Mark_Wegner_Strikes$plate_z > Mark_Wegner_Strikes$sz_bot & Mark_Wegner_Strikes$plate_z < Mark_Wegner_Strikes$sz_top), 'S', 'B')
table(Mark_Wegner_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mark_Wegner_Balls$AdjustedCall = ifelse((Mark_Wegner_Balls$plate_x > 0.833 | Mark_Wegner_Balls$plate_x < -0.833)|(Mark_Wegner_Balls$plate_z < Mark_Wegner_Balls$sz_bot | Mark_Wegner_Balls$plate_z > Mark_Wegner_Balls$sz_top),'B','S')
table(Mark_Wegner_Balls$AdjustedCall)

# Merge to create new dataset
Mark_Wegner_AdjustedCalls = rbind(Mark_Wegner_Strikes,Mark_Wegner_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Mark_Wegner)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Mark_Wegner_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Mark_Wegner_AdjustedCalls$AdjustedCall))







