# 6/1/18 : Giants vs __OPPONENT__ : Ed Hickox

# 131 pitches were called strikes/balls

# The robot-ump called 44 of those pitches as called strikes & 87 as balls

# Ed Hickox called 47 of those pitches as called strikes & 84 as balls 

# Accuracy: 93%

Ed_Hickox <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Ed_Hickox_6:1:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Ed_Hickox)
names(Ed_Hickox)
is.na(Ed_Hickox)
colSums(is.na(Ed_Hickox)) 
Ed_Hickox = Ed_Hickox[,colSums(is.na(Ed_Hickox)) == 0] 
dim(Ed_Hickox)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Ed_Hickox = Ed_Hickox[ , !(names(Ed_Hickox) %in% drops)]
dim(Ed_Hickox)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Ed_Hickox_train = Ed_Hickox[0:(0.8 * nrow(Ed_Hickox)),]
dim(Ed_Hickox_train)
prop.table(table(Ed_Hickox_train$type))
Ed_Hickox_test = Ed_Hickox[(0.8*nrow(Ed_Hickox)):nrow(Ed_Hickox),]
dim(Ed_Hickox_test)
prop.table(table(Ed_Hickox_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Ed_Hickox_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Ed_Hickox_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Ed_Hickox_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Ed_Hickox_Strikes = subset(Ed_Hickox, Ed_Hickox$type == "S")
Ed_Hickox_Balls = subset(Ed_Hickox, Ed_Hickox$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Ed_Hickox_Strikes$AdjustedCall = ifelse((Ed_Hickox_Strikes$plate_x < 0.833 & Ed_Hickox_Strikes$plate_x > -0.833) & (Ed_Hickox_Strikes$plate_z > Ed_Hickox_Strikes$sz_bot & Ed_Hickox_Strikes$plate_z < Ed_Hickox_Strikes$sz_top), 'S', 'B')
table(Ed_Hickox_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Ed_Hickox_Balls$AdjustedCall = ifelse((Ed_Hickox_Balls$plate_x > 0.833 | Ed_Hickox_Balls$plate_x < -0.833)|(Ed_Hickox_Balls$plate_z < Ed_Hickox_Balls$sz_bot | Ed_Hickox_Balls$plate_z > Ed_Hickox_Balls$sz_top),'B','S')
table(Ed_Hickox_Balls$AdjustedCall)

# Merge to create new dataset
Ed_Hickox_AdjustedCalls = rbind(Ed_Hickox_Strikes,Ed_Hickox_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Ed_Hickox)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Ed_Hickox_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Ed_Hickox_AdjustedCalls$AdjustedCall))







