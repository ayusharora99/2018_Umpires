# 6/5/18 : Giants vs __OPPONENT__ : Tony Randazzo

# 133 pitches were called strikes/balls

# The robot-ump called 35 of those pitches as called strikes & 98 as balls

# Tony Randazzo called 52 of those pitches as called strikes & 81 as balls 

# Accuracy: 87%

Tony_Randazzo <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Tony_Randazzo_6:5:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Tony_Randazzo)
names(Tony_Randazzo)
is.na(Tony_Randazzo)
colSums(is.na(Tony_Randazzo)) 
Tony_Randazzo = Tony_Randazzo[,colSums(is.na(Tony_Randazzo)) == 0] 
dim(Tony_Randazzo)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Tony_Randazzo = Tony_Randazzo[ , !(names(Tony_Randazzo) %in% drops)]
dim(Tony_Randazzo)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Tony_Randazzo_train = Tony_Randazzo[0:(0.8 * nrow(Tony_Randazzo)),]
dim(Tony_Randazzo_train)
prop.table(table(Tony_Randazzo_train$type))
Tony_Randazzo_test = Tony_Randazzo[(0.8*nrow(Tony_Randazzo)):nrow(Tony_Randazzo),]
dim(Tony_Randazzo_test)
prop.table(table(Tony_Randazzo_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Tony_Randazzo_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Tony_Randazzo_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Tony_Randazzo_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Tony_Randazzo_Strikes = subset(Tony_Randazzo, Tony_Randazzo$type == "S")
Tony_Randazzo_Balls = subset(Tony_Randazzo, Tony_Randazzo$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Tony_Randazzo_Strikes$AdjustedCall = ifelse((Tony_Randazzo_Strikes$plate_x < 0.833 & Tony_Randazzo_Strikes$plate_x > -0.833) & (Tony_Randazzo_Strikes$plate_z > Tony_Randazzo_Strikes$sz_bot & Tony_Randazzo_Strikes$plate_z < Tony_Randazzo_Strikes$sz_top), 'S', 'B')
table(Tony_Randazzo_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Tony_Randazzo_Balls$AdjustedCall = ifelse((Tony_Randazzo_Balls$plate_x > 0.833 | Tony_Randazzo_Balls$plate_x < -0.833)|(Tony_Randazzo_Balls$plate_z < Tony_Randazzo_Balls$sz_bot | Tony_Randazzo_Balls$plate_z > Tony_Randazzo_Balls$sz_top),'B','S')
table(Tony_Randazzo_Balls$AdjustedCall)

# Merge to create new dataset
Tony_Randazzo_AdjustedCalls = rbind(Tony_Randazzo_Strikes,Tony_Randazzo_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Tony_Randazzo)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Tony_Randazzo_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Tony_Randazzo_AdjustedCalls$AdjustedCall))







