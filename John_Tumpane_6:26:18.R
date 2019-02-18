# 6/26/18 : Giants vs __OPPONENT__ : John Tumpane

# 137 pitches were called strikes/balls

# The robot-ump called 39 of those pitches as called strikes & 98 as balls

# John Tumpane called 39 of those pitches as called strikes & 98 as balls 

# Accuracy: 94%

John_Tumpane <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/John_Tumpane_6:26:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(John_Tumpane)
names(John_Tumpane)
is.na(John_Tumpane)
colSums(is.na(John_Tumpane)) 
John_Tumpane = John_Tumpane[,colSums(is.na(John_Tumpane)) == 0] 
dim(John_Tumpane)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
John_Tumpane = John_Tumpane[ , !(names(John_Tumpane) %in% drops)]
dim(John_Tumpane)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
John_Tumpane_train = John_Tumpane[0:(0.8 * nrow(John_Tumpane)),]
dim(John_Tumpane_train)
prop.table(table(John_Tumpane_train$type))
John_Tumpane_test = John_Tumpane[(0.8*nrow(John_Tumpane)):nrow(John_Tumpane),]
dim(John_Tumpane_test)
prop.table(table(John_Tumpane_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = John_Tumpane_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = John_Tumpane_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, John_Tumpane_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
John_Tumpane_Strikes = subset(John_Tumpane, John_Tumpane$type == "S")
John_Tumpane_Balls = subset(John_Tumpane, John_Tumpane$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
John_Tumpane_Strikes$AdjustedCall = ifelse((John_Tumpane_Strikes$plate_x < 0.833 & John_Tumpane_Strikes$plate_x > -0.833) & (John_Tumpane_Strikes$plate_z > John_Tumpane_Strikes$sz_bot & John_Tumpane_Strikes$plate_z < John_Tumpane_Strikes$sz_top), 'S', 'B')
table(John_Tumpane_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
John_Tumpane_Balls$AdjustedCall = ifelse((John_Tumpane_Balls$plate_x > 0.833 | John_Tumpane_Balls$plate_x < -0.833)|(John_Tumpane_Balls$plate_z < John_Tumpane_Balls$sz_bot | John_Tumpane_Balls$plate_z > John_Tumpane_Balls$sz_top),'B','S')
table(John_Tumpane_Balls$AdjustedCall)

# Merge to create new dataset
John_Tumpane_AdjustedCalls = rbind(John_Tumpane_Strikes,John_Tumpane_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = John_Tumpane)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = John_Tumpane_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,John_Tumpane_AdjustedCalls$AdjustedCall))







