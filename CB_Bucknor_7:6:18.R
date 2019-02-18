# 7/6/18 : Giants vs Cardinals : CB Bucknor

# 125 pitches were called strikes/balls

# The robot-ump called 38 of those pitches as called strikes & 87 as balls

# CB Bucknor called 48 of those pitches as called strikes & 77 as balls 

# Accuracy: 87%

CB_Bucknor <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/CB_Bucknor_7:6:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(CB_Bucknor)
names(CB_Bucknor)
is.na(CB_Bucknor)
colSums(is.na(CB_Bucknor)) 
CB_Bucknor = CB_Bucknor[,colSums(is.na(CB_Bucknor)) == 0] 
dim(CB_Bucknor)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
CB_Bucknor = CB_Bucknor[ , !(names(CB_Bucknor) %in% drops)]
dim(CB_Bucknor)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
CB_Bucknor_train = CB_Bucknor[0:(0.8 * nrow(CB_Bucknor)),]
dim(CB_Bucknor_train)
prop.table(table(CB_Bucknor_train$type))
CB_Bucknor_test = CB_Bucknor[(0.8*nrow(CB_Bucknor)):nrow(CB_Bucknor),]
dim(CB_Bucknor_test)
prop.table(table(CB_Bucknor_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = CB_Bucknor_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = CB_Bucknor_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, CB_Bucknor_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
CB_Bucknor_Strikes = subset(CB_Bucknor, CB_Bucknor$type == "S")
CB_Bucknor_Balls = subset(CB_Bucknor, CB_Bucknor$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
CB_Bucknor_Strikes$AdjustedCall = ifelse((CB_Bucknor_Strikes$plate_x < 0.833 & CB_Bucknor_Strikes$plate_x > -0.833) & (CB_Bucknor_Strikes$plate_z > CB_Bucknor_Strikes$sz_bot & CB_Bucknor_Strikes$plate_z < CB_Bucknor_Strikes$sz_top), 'S', 'B')
table(CB_Bucknor_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
CB_Bucknor_Balls$AdjustedCall = ifelse((CB_Bucknor_Balls$plate_x > 0.833 | CB_Bucknor_Balls$plate_x < -0.833)|(CB_Bucknor_Balls$plate_z < CB_Bucknor_Balls$sz_bot | CB_Bucknor_Balls$plate_z > CB_Bucknor_Balls$sz_top),'B','S')
table(CB_Bucknor_Balls$AdjustedCall)

# Merge to create new dataset
CB_Bucknor_AdjustedCalls = rbind(CB_Bucknor_Strikes,CB_Bucknor_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = CB_Bucknor)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = CB_Bucknor_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,CB_Bucknor_AdjustedCalls$AdjustedCall))







