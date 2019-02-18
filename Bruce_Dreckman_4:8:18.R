# 4/8/18 : Giants vs __OPPONENT__ : Bruce Dreckman

# 142 pitches were called strikes/balls

# The robot-ump called 36 of those pitches as called strikes & 106 as balls

# Bruce Dreckman called 41 of those pitches as called strikes & 101 as balls 

# Accuracy: 92% 

Bruce_Dreckman <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Bruce_Dreckman_4:4:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Bruce_Dreckman)
names(Bruce_Dreckman)
is.na(Bruce_Dreckman)
colSums(is.na(Bruce_Dreckman)) 
Bruce_Dreckman = Bruce_Dreckman[,colSums(is.na(Bruce_Dreckman)) == 0] 
dim(Bruce_Dreckman)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Bruce_Dreckman = Bruce_Dreckman[ , !(names(Bruce_Dreckman) %in% drops)]
dim(Bruce_Dreckman)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Bruce_Dreckman_train = Bruce_Dreckman[0:(0.8 * nrow(Bruce_Dreckman)),]
dim(Bruce_Dreckman_train)
prop.table(table(Bruce_Dreckman_train$type))
Bruce_Dreckman_test = Bruce_Dreckman[(0.8*nrow(Bruce_Dreckman)):nrow(Bruce_Dreckman),]
dim(Bruce_Dreckman_test)
prop.table(table(Bruce_Dreckman_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Bruce_Dreckman_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Bruce_Dreckman_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Bruce_Dreckman_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Bruce_Dreckman_Strikes = subset(Bruce_Dreckman, Bruce_Dreckman$type == "S")
Bruce_Dreckman_Balls = subset(Bruce_Dreckman, Bruce_Dreckman$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Bruce_Dreckman_Strikes$AdjustedCall = ifelse((Bruce_Dreckman_Strikes$plate_x < 0.833 & Bruce_Dreckman_Strikes$plate_x > -0.833) & (Bruce_Dreckman_Strikes$plate_z > Bruce_Dreckman_Strikes$sz_bot & Bruce_Dreckman_Strikes$plate_z < Bruce_Dreckman_Strikes$sz_top), 'S', 'B')
table(Bruce_Dreckman_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Bruce_Dreckman_Balls$AdjustedCall = ifelse((Bruce_Dreckman_Balls$plate_x > 0.833 | Bruce_Dreckman_Balls$plate_x < -0.833)|(Bruce_Dreckman_Balls$plate_z < Bruce_Dreckman_Balls$sz_bot | Bruce_Dreckman_Balls$plate_z > Bruce_Dreckman_Balls$sz_top),'B','S')
table(Bruce_Dreckman_Balls$AdjustedCall)

# Merge to create new dataset
Bruce_Dreckman_AdjustedCalls = rbind(Bruce_Dreckman_Strikes,Bruce_Dreckman_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Bruce_Dreckman)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Bruce_Dreckman_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Bruce_Dreckman_AdjustedCalls$AdjustedCall))







