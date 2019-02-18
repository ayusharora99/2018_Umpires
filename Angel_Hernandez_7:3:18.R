# 7/3/18 : Giants vs Rockies : Angel Hernandez

# 132 pitches were called strikes/balls

# The robot-ump called 41 of those pitches as called strikes & 91 as balls

# Angel Hernandez called 52 of those pitches as called strikes & 80 as balls 

# Accuracy: 89%

Angel_Hernandez <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Angel_Hernandez_7:3:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Angel_Hernandez)
names(Angel_Hernandez)
is.na(Angel_Hernandez)
colSums(is.na(Angel_Hernandez)) 
Angel_Hernandez = Angel_Hernandez[,colSums(is.na(Angel_Hernandez)) == 0] 
dim(Angel_Hernandez)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Angel_Hernandez = Angel_Hernandez[ , !(names(Angel_Hernandez) %in% drops)]
dim(Angel_Hernandez)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Angel_Hernandez_train = Angel_Hernandez[0:(0.8 * nrow(Angel_Hernandez)),]
dim(Angel_Hernandez_train)
prop.table(table(Angel_Hernandez_train$type))
Angel_Hernandez_test = Angel_Hernandez[(0.8*nrow(Angel_Hernandez)):nrow(Angel_Hernandez),]
dim(Angel_Hernandez_test)
prop.table(table(Angel_Hernandez_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Angel_Hernandez_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Angel_Hernandez_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Angel_Hernandez_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Angel_Hernandez_Strikes = subset(Angel_Hernandez, Angel_Hernandez$type == "S")
Angel_Hernandez_Balls = subset(Angel_Hernandez, Angel_Hernandez$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Angel_Hernandez_Strikes$AdjustedCall = ifelse((Angel_Hernandez_Strikes$plate_x < 0.833 & Angel_Hernandez_Strikes$plate_x > -0.833) & (Angel_Hernandez_Strikes$plate_z > Angel_Hernandez_Strikes$sz_bot & Angel_Hernandez_Strikes$plate_z < Angel_Hernandez_Strikes$sz_top), 'S', 'B')
table(Angel_Hernandez_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Angel_Hernandez_Balls$AdjustedCall = ifelse((Angel_Hernandez_Balls$plate_x > 0.833 | Angel_Hernandez_Balls$plate_x < -0.833)|(Angel_Hernandez_Balls$plate_z < Angel_Hernandez_Balls$sz_bot | Angel_Hernandez_Balls$plate_z > Angel_Hernandez_Balls$sz_top),'B','S')
table(Angel_Hernandez_Balls$AdjustedCall)

# Merge to create new dataset
Angel_Hernandez_AdjustedCalls = rbind(Angel_Hernandez_Strikes,Angel_Hernandez_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Angel_Hernandez)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Angel_Hernandez_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Angel_Hernandez_AdjustedCalls$AdjustedCall))







