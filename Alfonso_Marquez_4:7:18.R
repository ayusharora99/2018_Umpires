# 4/7/18 : Giants vs __OPPONENT__ : Alfonso_Marquez

# 260 pitches were called strikes/balls

# The robot-ump called 75 of those pitches as called strikes & 190 as balls

# Alfonso_Marquez called 90 of those pitches as called strikes & 174 as balls 

# Accuracy: 94% 

Alfonso_Marquez <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Alfonso_Marquez_4:7:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Alfonso_Marquez)
names(Alfonso_Marquez)
is.na(Alfonso_Marquez)
colSums(is.na(Alfonso_Marquez)) 
Alfonso_Marquez = Alfonso_Marquez[,colSums(is.na(Alfonso_Marquez)) == 0] 
dim(Alfonso_Marquez)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Alfonso_Marquez = Alfonso_Marquez[ , !(names(Alfonso_Marquez) %in% drops)]
dim(Alfonso_Marquez)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Alfonso_Marquez_train = Alfonso_Marquez[0:(0.8 * nrow(Alfonso_Marquez)),]
dim(Alfonso_Marquez_train)
prop.table(table(Alfonso_Marquez_train$type))
Alfonso_Marquez_test = Alfonso_Marquez[(0.8*nrow(Alfonso_Marquez)):nrow(Alfonso_Marquez),]
dim(Alfonso_Marquez_test)
prop.table(table(Alfonso_Marquez_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Alfonso_Marquez_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Alfonso_Marquez_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Alfonso_Marquez_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Alfonso_Marquez_Strikes = subset(Alfonso_Marquez, Alfonso_Marquez$type == "S")
Alfonso_Marquez_Balls = subset(Alfonso_Marquez, Alfonso_Marquez$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Alfonso_Marquez_Strikes$AdjustedCall = ifelse((Alfonso_Marquez_Strikes$plate_x < 0.833 & Alfonso_Marquez_Strikes$plate_x > -0.833) & (Alfonso_Marquez_Strikes$plate_z > Alfonso_Marquez_Strikes$sz_bot & Alfonso_Marquez_Strikes$plate_z < Alfonso_Marquez_Strikes$sz_top), 'S', 'B')
table(Alfonso_Marquez_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Alfonso_Marquez_Balls$AdjustedCall = ifelse((Alfonso_Marquez_Balls$plate_x > 0.833 | Alfonso_Marquez_Balls$plate_x < -0.833)|(Alfonso_Marquez_Balls$plate_z < Alfonso_Marquez_Balls$sz_bot | Alfonso_Marquez_Balls$plate_z > Alfonso_Marquez_Balls$sz_top),'B','S')
table(Alfonso_Marquez_Balls$AdjustedCall)

# Merge to create new dataset
Alfonso_Marquez_AdjustedCalls = rbind(Alfonso_Marquez_Strikes,Alfonso_Marquez_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Alfonso_Marquez)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Alfonso_Marquez_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Alfonso_Marquez_AdjustedCalls$AdjustedCall))







