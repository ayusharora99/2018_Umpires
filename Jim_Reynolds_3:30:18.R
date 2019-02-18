# 3/30/18 : Giants vs Dodgers : Jim Reynolds

# 115 pitches were called strikes/balls

# The robot-ump called 33 of those pitches as called strikes & 82 as balls

# Jim Reynolds called 43 of those pitches as called strikes & 72 as balls 

# Accuracy: 86%

Jim_Reynolds <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jim_Reynolds_3:30:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Jim_Reynolds)
names(Jim_Reynolds)
is.na(Jim_Reynolds)
colSums(is.na(Jim_Reynolds)) 
Jim_Reynolds = Jim_Reynolds[,colSums(is.na(Jim_Reynolds)) == 0] 
dim(Jim_Reynolds)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jim_Reynolds = Jim_Reynolds[ , !(names(Jim_Reynolds) %in% drops)]
dim(Jim_Reynolds)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Jim_Reynolds_train = Jim_Reynolds[0:(0.8 * nrow(Jim_Reynolds)),]
dim(Jim_Reynolds_train)
prop.table(table(Jim_Reynolds_train$type))
Jim_Reynolds_test = Jim_Reynolds[(0.8*nrow(Jim_Reynolds)):nrow(Jim_Reynolds),]
dim(Jim_Reynolds_test)
prop.table(table(Jim_Reynolds_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jim_Reynolds_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Jim_Reynolds_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Jim_Reynolds_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Jim_Reynolds_Strikes = subset(Jim_Reynolds, Jim_Reynolds$type == "S")
Jim_Reynolds_Balls = subset(Jim_Reynolds, Jim_Reynolds$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jim_Reynolds_Strikes$AdjustedCall = ifelse((Jim_Reynolds_Strikes$plate_x < 0.833 & Jim_Reynolds_Strikes$plate_x > -0.833) & (Jim_Reynolds_Strikes$plate_z > Jim_Reynolds_Strikes$sz_bot & Jim_Reynolds_Strikes$plate_z < Jim_Reynolds_Strikes$sz_top), 'S', 'B')
table(Jim_Reynolds_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jim_Reynolds_Balls$AdjustedCall = ifelse((Jim_Reynolds_Balls$plate_x > 0.833 | Jim_Reynolds_Balls$plate_x < -0.833)|(Jim_Reynolds_Balls$plate_z < Jim_Reynolds_Balls$sz_bot | Jim_Reynolds_Balls$plate_z > Jim_Reynolds_Balls$sz_top),'B','S')
table(Jim_Reynolds_Balls$AdjustedCall)

# Merge to create new dataset
Jim_Reynolds_AdjustedCalls = rbind(Jim_Reynolds_Strikes,Jim_Reynolds_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Jim_Reynolds)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Jim_Reynolds_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Jim_Reynolds_AdjustedCalls$AdjustedCall))







