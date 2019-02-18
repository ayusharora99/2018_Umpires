# 6/30/18 : Giants vs __OPPONENT__ : Chad Whitson

# 147 pitches were called strikes/balls

# The robot-ump called 47 of those pitches as called strikes & 100 as balls

# Chad Whitson called 56 of those pitches as called strikes & 91 as balls 

# Accuracy: 90%

Chad_Whitson <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chad_Whitson_6:30:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Chad_Whitson)
names(Chad_Whitson)
is.na(Chad_Whitson)
colSums(is.na(Chad_Whitson)) 
Chad_Whitson = Chad_Whitson[,colSums(is.na(Chad_Whitson)) == 0] 
dim(Chad_Whitson)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Chad_Whitson = Chad_Whitson[ , !(names(Chad_Whitson) %in% drops)]
dim(Chad_Whitson)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Chad_Whitson_train = Chad_Whitson[0:(0.8 * nrow(Chad_Whitson)),]
dim(Chad_Whitson_train)
prop.table(table(Chad_Whitson_train$type))
Chad_Whitson_test = Chad_Whitson[(0.8*nrow(Chad_Whitson)):nrow(Chad_Whitson),]
dim(Chad_Whitson_test)
prop.table(table(Chad_Whitson_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Chad_Whitson_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Chad_Whitson_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Chad_Whitson_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Chad_Whitson_Strikes = subset(Chad_Whitson, Chad_Whitson$type == "S")
Chad_Whitson_Balls = subset(Chad_Whitson, Chad_Whitson$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Chad_Whitson_Strikes$AdjustedCall = ifelse((Chad_Whitson_Strikes$plate_x < 0.833 & Chad_Whitson_Strikes$plate_x > -0.833) & (Chad_Whitson_Strikes$plate_z > Chad_Whitson_Strikes$sz_bot & Chad_Whitson_Strikes$plate_z < Chad_Whitson_Strikes$sz_top), 'S', 'B')
table(Chad_Whitson_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Chad_Whitson_Balls$AdjustedCall = ifelse((Chad_Whitson_Balls$plate_x > 0.833 | Chad_Whitson_Balls$plate_x < -0.833)|(Chad_Whitson_Balls$plate_z < Chad_Whitson_Balls$sz_bot | Chad_Whitson_Balls$plate_z > Chad_Whitson_Balls$sz_top),'B','S')
table(Chad_Whitson_Balls$AdjustedCall)

# Merge to create new dataset
Chad_Whitson_AdjustedCalls = rbind(Chad_Whitson_Strikes,Chad_Whitson_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Chad_Whitson)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Chad_Whitson_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Chad_Whitson_AdjustedCalls$AdjustedCall))







