# 6/19/18 : Giants vs __OPPONENT__ : Andy Fletcher

# 150 pitches were called strikes/balls

# The robot-ump called 41 of those pitches as called strikes & 109 as balls

# Andy Fletcher called 44 of those pitches as called strikes & 106 as balls 

# Accuracy: 90%

Andy_Fletcher <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Andy_Fletcher_6:19:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Andy_Fletcher)
names(Andy_Fletcher)
is.na(Andy_Fletcher)
colSums(is.na(Andy_Fletcher)) 
Andy_Fletcher = Andy_Fletcher[,colSums(is.na(Andy_Fletcher)) == 0] 
dim(Andy_Fletcher)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Andy_Fletcher = Andy_Fletcher[ , !(names(Andy_Fletcher) %in% drops)]
dim(Andy_Fletcher)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Andy_Fletcher_train = Andy_Fletcher[0:(0.8 * nrow(Andy_Fletcher)),]
dim(Andy_Fletcher_train)
prop.table(table(Andy_Fletcher_train$type))
Andy_Fletcher_test = Andy_Fletcher[(0.8*nrow(Andy_Fletcher)):nrow(Andy_Fletcher),]
dim(Andy_Fletcher_test)
prop.table(table(Andy_Fletcher_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Andy_Fletcher_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Andy_Fletcher_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Andy_Fletcher_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Andy_Fletcher_Strikes = subset(Andy_Fletcher, Andy_Fletcher$type == "S")
Andy_Fletcher_Balls = subset(Andy_Fletcher, Andy_Fletcher$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Andy_Fletcher_Strikes$AdjustedCall = ifelse((Andy_Fletcher_Strikes$plate_x < 0.833 & Andy_Fletcher_Strikes$plate_x > -0.833) & (Andy_Fletcher_Strikes$plate_z > Andy_Fletcher_Strikes$sz_bot & Andy_Fletcher_Strikes$plate_z < Andy_Fletcher_Strikes$sz_top), 'S', 'B')
table(Andy_Fletcher_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Andy_Fletcher_Balls$AdjustedCall = ifelse((Andy_Fletcher_Balls$plate_x > 0.833 | Andy_Fletcher_Balls$plate_x < -0.833)|(Andy_Fletcher_Balls$plate_z < Andy_Fletcher_Balls$sz_bot | Andy_Fletcher_Balls$plate_z > Andy_Fletcher_Balls$sz_top),'B','S')
table(Andy_Fletcher_Balls$AdjustedCall)

# Merge to create new dataset
Andy_Fletcher_AdjustedCalls = rbind(Andy_Fletcher_Strikes,Andy_Fletcher_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Andy_Fletcher)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Andy_Fletcher_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Andy_Fletcher_AdjustedCalls$AdjustedCall))







