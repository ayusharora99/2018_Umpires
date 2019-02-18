# 4/19/18 : Giants vs __OPPONENT__ : Tom_Hallion

# 127 pitches were called strikes/balls

# The robot-ump called 32 of those pitches as called strikes & 95 as balls

# Tom_Hallion called 42 of those pitches as called strikes & 85 as balls 

# Accuracy: 91% 

Tom_Hallion <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Tom Hallion_4:19:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Tom_Hallion)
names(Tom_Hallion)
is.na(Tom_Hallion)
colSums(is.na(Tom_Hallion)) 
Tom_Hallion = Tom_Hallion[,colSums(is.na(Tom_Hallion)) == 0] 
dim(Tom_Hallion)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Tom_Hallion = Tom_Hallion[ , !(names(Tom_Hallion) %in% drops)]
dim(Tom_Hallion)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Tom_Hallion_train = Tom_Hallion[0:(0.8 * nrow(Tom_Hallion)),]
dim(Tom_Hallion_train)
prop.table(table(Tom_Hallion_train$type))
Tom_Hallion_test = Tom_Hallion[(0.8*nrow(Tom_Hallion)):nrow(Tom_Hallion),]
dim(Tom_Hallion_test)
prop.table(table(Tom_Hallion_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Tom_Hallion_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Tom_Hallion_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Tom_Hallion_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Tom_Hallion_Strikes = subset(Tom_Hallion, Tom_Hallion$type == "S")
Tom_Hallion_Balls = subset(Tom_Hallion, Tom_Hallion$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Tom_Hallion_Strikes$AdjustedCall = ifelse((Tom_Hallion_Strikes$plate_x < 0.833 & Tom_Hallion_Strikes$plate_x > -0.833) & (Tom_Hallion_Strikes$plate_z > Tom_Hallion_Strikes$sz_bot & Tom_Hallion_Strikes$plate_z < Tom_Hallion_Strikes$sz_top), 'S', 'B')
table(Tom_Hallion_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Tom_Hallion_Balls$AdjustedCall = ifelse((Tom_Hallion_Balls$plate_x > 0.833 | Tom_Hallion_Balls$plate_x < -0.833)|(Tom_Hallion_Balls$plate_z < Tom_Hallion_Balls$sz_bot | Tom_Hallion_Balls$plate_z > Tom_Hallion_Balls$sz_top),'B','S')
table(Tom_Hallion_Balls$AdjustedCall)

# Merge to create new dataset
Tom_Hallion_AdjustedCalls = rbind(Tom_Hallion_Strikes,Tom_Hallion_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Tom_Hallion)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Tom_Hallion_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Tom_Hallion_AdjustedCalls$AdjustedCall))







