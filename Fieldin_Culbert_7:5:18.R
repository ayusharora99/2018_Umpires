# 7/5/18 : Giants vs Cardinals : Fieldin Culbreth

# 120 pitches were called strikes/balls

# The robot-ump called 46 of those pitches as called strikes & 74 as balls

# Fieldin Culberth called 49 of those pitches as called strikes & 71 as balls 

# Accuracy: 90%

Fieldin_Culbreth <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Fieldin_Culbreth_7:5:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Fieldin_Culbreth)
names(Fieldin_Culbreth)
is.na(Fieldin_Culbreth)
colSums(is.na(Fieldin_Culbreth)) 
Fieldin_Culbreth = Fieldin_Culbreth[,colSums(is.na(Fieldin_Culbreth)) == 0] 
dim(Fieldin_Culbreth)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Fieldin_Culbreth = Fieldin_Culbreth[ , !(names(Fieldin_Culbreth) %in% drops)]
dim(Fieldin_Culbreth)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Fieldin_Culbreth_train = Fieldin_Culbreth[0:(0.8 * nrow(Fieldin_Culbreth)),]
dim(Fieldin_Culbreth_train)
prop.table(table(Fieldin_Culbreth_train$type))
Fieldin_Culbreth_test = Fieldin_Culbreth[(0.8*nrow(Fieldin_Culbreth)):nrow(Fieldin_Culbreth),]
dim(Fieldin_Culbreth_test)
prop.table(table(Fieldin_Culbreth_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Fieldin_Culbreth_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Fieldin_Culbreth_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Fieldin_Culbreth_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Fieldin_Culbreth_Strikes = subset(Fieldin_Culbreth, Fieldin_Culbreth$type == "S")
Fieldin_Culbreth_Balls = subset(Fieldin_Culbreth, Fieldin_Culbreth$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Fieldin_Culbreth_Strikes$AdjustedCall = ifelse((Fieldin_Culbreth_Strikes$plate_x < 0.833 & Fieldin_Culbreth_Strikes$plate_x > -0.833) & (Fieldin_Culbreth_Strikes$plate_z > Fieldin_Culbreth_Strikes$sz_bot & Fieldin_Culbreth_Strikes$plate_z < Fieldin_Culbreth_Strikes$sz_top), 'S', 'B')
table(Fieldin_Culbreth_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Fieldin_Culbreth_Balls$AdjustedCall = ifelse((Fieldin_Culbreth_Balls$plate_x > 0.833 | Fieldin_Culbreth_Balls$plate_x < -0.833)|(Fieldin_Culbreth_Balls$plate_z < Fieldin_Culbreth_Balls$sz_bot | Fieldin_Culbreth_Balls$plate_z > Fieldin_Culbreth_Balls$sz_top),'B','S')
table(Fieldin_Culbreth_Balls$AdjustedCall)

# Merge to create new dataset
Fieldin_Culbreth_AdjustedCalls = rbind(Fieldin_Culbreth_Strikes,Fieldin_Culbreth_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Fieldin_Culbreth)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Fieldin_Culbreth_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Fieldin_Culbreth_AdjustedCalls$AdjustedCall))







