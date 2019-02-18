# 6/6/18 : Giants vs __OPPONENT__ : Bill Welke

# 165 pitches were called strikes/balls

# The robot-ump called 50 of those pitches as called strikes & 115 as balls

# Bill Welke called 59 of those pitches as called strikes & 106 as balls 

# Accuracy: 91%

Bill_Welke <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Bill_Welke_6:6:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Bill_Welke)
names(Bill_Welke)
is.na(Bill_Welke)
colSums(is.na(Bill_Welke)) 
Bill_Welke = Bill_Welke[,colSums(is.na(Bill_Welke)) == 0] 
dim(Bill_Welke)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Bill_Welke = Bill_Welke[ , !(names(Bill_Welke) %in% drops)]
dim(Bill_Welke)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Bill_Welke_train = Bill_Welke[0:(0.8 * nrow(Bill_Welke)),]
dim(Bill_Welke_train)
prop.table(table(Bill_Welke_train$type))
Bill_Welke_test = Bill_Welke[(0.8*nrow(Bill_Welke)):nrow(Bill_Welke),]
dim(Bill_Welke_test)
prop.table(table(Bill_Welke_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Bill_Welke_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Bill_Welke_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Bill_Welke_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Bill_Welke_Strikes = subset(Bill_Welke, Bill_Welke$type == "S")
Bill_Welke_Balls = subset(Bill_Welke, Bill_Welke$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Bill_Welke_Strikes$AdjustedCall = ifelse((Bill_Welke_Strikes$plate_x < 0.833 & Bill_Welke_Strikes$plate_x > -0.833) & (Bill_Welke_Strikes$plate_z > Bill_Welke_Strikes$sz_bot & Bill_Welke_Strikes$plate_z < Bill_Welke_Strikes$sz_top), 'S', 'B')
table(Bill_Welke_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Bill_Welke_Balls$AdjustedCall = ifelse((Bill_Welke_Balls$plate_x > 0.833 | Bill_Welke_Balls$plate_x < -0.833)|(Bill_Welke_Balls$plate_z < Bill_Welke_Balls$sz_bot | Bill_Welke_Balls$plate_z > Bill_Welke_Balls$sz_top),'B','S')
table(Bill_Welke_Balls$AdjustedCall)

# Merge to create new dataset
Bill_Welke_AdjustedCalls = rbind(Bill_Welke_Strikes,Bill_Welke_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Bill_Welke)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Bill_Welke_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Bill_Welke_AdjustedCalls$AdjustedCall))







