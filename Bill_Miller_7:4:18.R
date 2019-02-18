# 7/4/18 : Giants vs Rockies : Bill Miller

# 108 pitches were called strikes/balls

# The robot-ump called 29 of those pitches as called strikes & 79 as balls

# Bill Miller called 30 of those pitches as called strikes & 78 as balls 

# Accuracy: 88%

Bill_Miller <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Bill_Miller_7:4:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Bill_Miller)
names(Bill_Miller)
is.na(Bill_Miller)
colSums(is.na(Bill_Miller)) 
Bill_Miller = Bill_Miller[,colSums(is.na(Bill_Miller)) == 0] 
dim(Bill_Miller)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Bill_Miller = Bill_Miller[ , !(names(Bill_Miller) %in% drops)]
dim(Bill_Miller)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Bill_Miller_train = Bill_Miller[0:(0.8 * nrow(Bill_Miller)),]
dim(Bill_Miller_train)
prop.table(table(Bill_Miller_train$type))
Bill_Miller_test = Bill_Miller[(0.8*nrow(Bill_Miller)):nrow(Bill_Miller),]
dim(Bill_Miller_test)
prop.table(table(Bill_Miller_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Bill_Miller_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Bill_Miller_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Bill_Miller_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Bill_Miller_Strikes = subset(Bill_Miller, Bill_Miller$type == "S")
Bill_Miller_Balls = subset(Bill_Miller, Bill_Miller$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Bill_Miller_Strikes$AdjustedCall = ifelse((Bill_Miller_Strikes$plate_x < 0.833 & Bill_Miller_Strikes$plate_x > -0.833) & (Bill_Miller_Strikes$plate_z > Bill_Miller_Strikes$sz_bot & Bill_Miller_Strikes$plate_z < Bill_Miller_Strikes$sz_top), 'S', 'B')
table(Bill_Miller_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Bill_Miller_Balls$AdjustedCall = ifelse((Bill_Miller_Balls$plate_x > 0.833 | Bill_Miller_Balls$plate_x < -0.833)|(Bill_Miller_Balls$plate_z < Bill_Miller_Balls$sz_bot | Bill_Miller_Balls$plate_z > Bill_Miller_Balls$sz_top),'B','S')
table(Bill_Miller_Balls$AdjustedCall)

# Merge to create new dataset
Bill_Miller_AdjustedCalls = rbind(Bill_Miller_Strikes,Bill_Miller_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Bill_Miller)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Bill_Miller_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Bill_Miller_AdjustedCalls$AdjustedCall))







