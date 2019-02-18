# 4/28/18 : Giants vs __OPPONENT__ : Trip Gibson

# 191 pitches were called strikes/balls

# The robot-ump called 41 of those pitches as called strikes & 150 as balls

# Trip Gibson called 59 of those pitches as called strikes & 132 as balls 

# Accuracy: 90% 

Trip_Gibson <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Trip_Gibson_4:28:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Trip_Gibson)
names(Trip_Gibson)
is.na(Trip_Gibson)
colSums(is.na(Trip_Gibson)) 
Trip_Gibson = Trip_Gibson[,colSums(is.na(Trip_Gibson)) == 0] 
dim(Trip_Gibson)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Trip_Gibson = Trip_Gibson[ , !(names(Trip_Gibson) %in% drops)]
dim(Trip_Gibson)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Trip_Gibson_train = Trip_Gibson[0:(0.8 * nrow(Trip_Gibson)),]
dim(Trip_Gibson_train)
prop.table(table(Trip_Gibson_train$type))
Trip_Gibson_test = Trip_Gibson[(0.8*nrow(Trip_Gibson)):nrow(Trip_Gibson),]
dim(Trip_Gibson_test)
prop.table(table(Trip_Gibson_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Trip_Gibson_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Trip_Gibson_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Trip_Gibson_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Trip_Gibson_Strikes = subset(Trip_Gibson, Trip_Gibson$type == "S")
Trip_Gibson_Balls = subset(Trip_Gibson, Trip_Gibson$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Trip_Gibson_Strikes$AdjustedCall = ifelse((Trip_Gibson_Strikes$plate_x < 0.833 & Trip_Gibson_Strikes$plate_x > -0.833) & (Trip_Gibson_Strikes$plate_z > Trip_Gibson_Strikes$sz_bot & Trip_Gibson_Strikes$plate_z < Trip_Gibson_Strikes$sz_top), 'S', 'B')
table(Trip_Gibson_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Trip_Gibson_Balls$AdjustedCall = ifelse((Trip_Gibson_Balls$plate_x > 0.833 | Trip_Gibson_Balls$plate_x < -0.833)|(Trip_Gibson_Balls$plate_z < Trip_Gibson_Balls$sz_bot | Trip_Gibson_Balls$plate_z > Trip_Gibson_Balls$sz_top),'B','S')
table(Trip_Gibson_Balls$AdjustedCall)

# Merge to create new dataset
Trip_Gibson_AdjustedCalls = rbind(Trip_Gibson_Strikes,Trip_Gibson_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Trip_Gibson)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Trip_Gibson_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Trip_Gibson_AdjustedCalls$AdjustedCall))







