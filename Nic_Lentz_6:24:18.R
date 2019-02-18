# 6/24/18 : Giants vs __OPPONENT__ : Nic Lentz

# 153 pitches were called strikes/balls

# The robot-ump called 46 of those pitches as called strikes & 107 as balls

# Nic Lentz called 55 of those pitches as called strikes & 98 as balls 

# Accuracy: 93%

Nic_Lentz <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Nic_Lentz_6:24:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Nic_Lentz)
names(Nic_Lentz)
is.na(Nic_Lentz)
colSums(is.na(Nic_Lentz)) 
Nic_Lentz = Nic_Lentz[,colSums(is.na(Nic_Lentz)) == 0] 
dim(Nic_Lentz)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Nic_Lentz = Nic_Lentz[ , !(names(Nic_Lentz) %in% drops)]
dim(Nic_Lentz)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Nic_Lentz_train = Nic_Lentz[0:(0.8 * nrow(Nic_Lentz)),]
dim(Nic_Lentz_train)
prop.table(table(Nic_Lentz_train$type))
Nic_Lentz_test = Nic_Lentz[(0.8*nrow(Nic_Lentz)):nrow(Nic_Lentz),]
dim(Nic_Lentz_test)
prop.table(table(Nic_Lentz_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Nic_Lentz_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Nic_Lentz_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Nic_Lentz_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Nic_Lentz_Strikes = subset(Nic_Lentz, Nic_Lentz$type == "S")
Nic_Lentz_Balls = subset(Nic_Lentz, Nic_Lentz$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Nic_Lentz_Strikes$AdjustedCall = ifelse((Nic_Lentz_Strikes$plate_x < 0.833 & Nic_Lentz_Strikes$plate_x > -0.833) & (Nic_Lentz_Strikes$plate_z > Nic_Lentz_Strikes$sz_bot & Nic_Lentz_Strikes$plate_z < Nic_Lentz_Strikes$sz_top), 'S', 'B')
table(Nic_Lentz_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Nic_Lentz_Balls$AdjustedCall = ifelse((Nic_Lentz_Balls$plate_x > 0.833 | Nic_Lentz_Balls$plate_x < -0.833)|(Nic_Lentz_Balls$plate_z < Nic_Lentz_Balls$sz_bot | Nic_Lentz_Balls$plate_z > Nic_Lentz_Balls$sz_top),'B','S')
table(Nic_Lentz_Balls$AdjustedCall)

# Merge to create new dataset
Nic_Lentz_AdjustedCalls = rbind(Nic_Lentz_Strikes,Nic_Lentz_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Nic_Lentz)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Nic_Lentz_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Nic_Lentz_AdjustedCalls$AdjustedCall))







