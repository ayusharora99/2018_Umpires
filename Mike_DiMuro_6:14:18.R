# 6/14/18 : Giants vs __OPPONENT__ : Mike DiMuro

# 239 pitches were called strikes/balls

# The robot-ump called 70 of those pitches as called strikes & 169 as balls

# Mike DiMuro called 77 of those pitches as called strikes & 162 as balls 

# Accuracy: 93%



Mike_DiMuro <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mike_DiMuro_6:14:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Mike_DiMuro)
names(Mike_DiMuro)
is.na(Mike_DiMuro)
colSums(is.na(Mike_DiMuro)) 
Mike_DiMuro = Mike_DiMuro[,colSums(is.na(Mike_DiMuro)) == 0] 
dim(Mike_DiMuro)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mike_DiMuro = Mike_DiMuro[ , !(names(Mike_DiMuro) %in% drops)]
dim(Mike_DiMuro)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Mike_DiMuro_train = Mike_DiMuro[0:(0.8 * nrow(Mike_DiMuro)),]
dim(Mike_DiMuro_train)
prop.table(table(Mike_DiMuro_train$type))
Mike_DiMuro_test = Mike_DiMuro[(0.8*nrow(Mike_DiMuro)):nrow(Mike_DiMuro),]
dim(Mike_DiMuro_test)
prop.table(table(Mike_DiMuro_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mike_DiMuro_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Mike_DiMuro_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Mike_DiMuro_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Mike_DiMuro_Strikes = subset(Mike_DiMuro, Mike_DiMuro$type == "S")
Mike_DiMuro_Balls = subset(Mike_DiMuro, Mike_DiMuro$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mike_DiMuro_Strikes$AdjustedCall = ifelse((Mike_DiMuro_Strikes$plate_x < 0.833 & Mike_DiMuro_Strikes$plate_x > -0.833) & (Mike_DiMuro_Strikes$plate_z > Mike_DiMuro_Strikes$sz_bot & Mike_DiMuro_Strikes$plate_z < Mike_DiMuro_Strikes$sz_top), 'S', 'B')
table(Mike_DiMuro_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mike_DiMuro_Balls$AdjustedCall = ifelse((Mike_DiMuro_Balls$plate_x > 0.833 | Mike_DiMuro_Balls$plate_x < -0.833)|(Mike_DiMuro_Balls$plate_z < Mike_DiMuro_Balls$sz_bot | Mike_DiMuro_Balls$plate_z > Mike_DiMuro_Balls$sz_top),'B','S')
table(Mike_DiMuro_Balls$AdjustedCall)

# Merge to create new dataset
Mike_DiMuro_AdjustedCalls = rbind(Mike_DiMuro_Strikes,Mike_DiMuro_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Mike_DiMuro)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Mike_DiMuro_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Mike_DiMuro_AdjustedCalls$AdjustedCall))







