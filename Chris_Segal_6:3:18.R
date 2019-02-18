# 6/3/18 : Giants vs __OPPONENT__ : Chris Segal

# 106 pitches were called strikes/balls

# The robot-ump called 33 of those pitches as called strikes & 73 as balls

# Chris Segal called 36 of those pitches as called strikes & 70 as balls 

# Accuracy: 90%

Chris_Segal <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chris_Segal_6:3:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Chris_Segal)
names(Chris_Segal)
is.na(Chris_Segal)
colSums(is.na(Chris_Segal)) 
Chris_Segal = Chris_Segal[,colSums(is.na(Chris_Segal)) == 0] 
dim(Chris_Segal)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Chris_Segal = Chris_Segal[ , !(names(Chris_Segal) %in% drops)]
dim(Chris_Segal)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Chris_Segal_train = Chris_Segal[0:(0.8 * nrow(Chris_Segal)),]
dim(Chris_Segal_train)
prop.table(table(Chris_Segal_train$type))
Chris_Segal_test = Chris_Segal[(0.8*nrow(Chris_Segal)):nrow(Chris_Segal),]
dim(Chris_Segal_test)
prop.table(table(Chris_Segal_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Chris_Segal_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Chris_Segal_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Chris_Segal_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Chris_Segal_Strikes = subset(Chris_Segal, Chris_Segal$type == "S")
Chris_Segal_Balls = subset(Chris_Segal, Chris_Segal$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Chris_Segal_Strikes$AdjustedCall = ifelse((Chris_Segal_Strikes$plate_x < 0.833 & Chris_Segal_Strikes$plate_x > -0.833) & (Chris_Segal_Strikes$plate_z > Chris_Segal_Strikes$sz_bot & Chris_Segal_Strikes$plate_z < Chris_Segal_Strikes$sz_top), 'S', 'B')
table(Chris_Segal_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Chris_Segal_Balls$AdjustedCall = ifelse((Chris_Segal_Balls$plate_x > 0.833 | Chris_Segal_Balls$plate_x < -0.833)|(Chris_Segal_Balls$plate_z < Chris_Segal_Balls$sz_bot | Chris_Segal_Balls$plate_z > Chris_Segal_Balls$sz_top),'B','S')
table(Chris_Segal_Balls$AdjustedCall)

# Merge to create new dataset
Chris_Segal_AdjustedCalls = rbind(Chris_Segal_Strikes,Chris_Segal_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Chris_Segal)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Chris_Segal_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Chris_Segal_AdjustedCalls$AdjustedCall))







