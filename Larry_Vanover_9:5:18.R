# 9/5/18 : Giants vs Rockies : Larry Vanover

# 129 pitches were called strikes/balls

# The robot-ump called 34 of those pitches as called strikes & 95 as balls

# Larry Vanover called 41 of those pitches as called strikes & 88 as balls 

# Accuracy: 93%

Larry_Vanover <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Larry_Vanover_9:5:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Larry_Vanover)
names(Larry_Vanover)
is.na(Larry_Vanover)
colSums(is.na(Larry_Vanover)) 
Larry_Vanover = Larry_Vanover[,colSums(is.na(Larry_Vanover)) == 0] 
dim(Larry_Vanover)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Larry_Vanover = Larry_Vanover[ , !(names(Larry_Vanover) %in% drops)]
dim(Larry_Vanover)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Larry_Vanover_train = Larry_Vanover[0:(0.8 * nrow(Larry_Vanover)),]
dim(Larry_Vanover_train)
prop.table(table(Larry_Vanover_train$type))
Larry_Vanover_test = Larry_Vanover[(0.8*nrow(Larry_Vanover)):nrow(Larry_Vanover),]
dim(Larry_Vanover_test)
prop.table(table(Larry_Vanover_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Larry_Vanover_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Larry_Vanover_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Larry_Vanover_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Larry_Vanover_Strikes = subset(Larry_Vanover, Larry_Vanover$type == "S")
Larry_Vanover_Balls = subset(Larry_Vanover, Larry_Vanover$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Larry_Vanover_Strikes$AdjustedCall = ifelse((Larry_Vanover_Strikes$plate_x < 0.833 & Larry_Vanover_Strikes$plate_x > -0.833) & (Larry_Vanover_Strikes$plate_z > Larry_Vanover_Strikes$sz_bot & Larry_Vanover_Strikes$plate_z < Larry_Vanover_Strikes$sz_top), 'S', 'B')
table(Larry_Vanover_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Larry_Vanover_Balls$AdjustedCall = ifelse((Larry_Vanover_Balls$plate_x > 0.833 | Larry_Vanover_Balls$plate_x < -0.833)|(Larry_Vanover_Balls$plate_z < Larry_Vanover_Balls$sz_bot | Larry_Vanover_Balls$plate_z > Larry_Vanover_Balls$sz_top),'B','S')
table(Larry_Vanover_Balls$AdjustedCall)

# Merge to create new dataset
Larry_Vanover_AdjustedCalls = rbind(Larry_Vanover_Strikes,Larry_Vanover_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Larry_Vanover)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Larry_Vanover_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Larry_Vanover_AdjustedCalls$AdjustedCall))







