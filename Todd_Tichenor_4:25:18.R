# 4/25/18 : Giants vs __OPPONENT__ : Todd Tichenor

# 150 pitches were called strikes/balls

# The robot-ump called 44 of those pitches as called strikes & 106 as balls

# Todd Tichenor called 52 of those pitches as called strikes & 98 as balls 

# Accuracy: 92% 

Todd_Tichenor <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Todd_Tichenor_4:25:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Todd_Tichenor)
names(Todd_Tichenor)
is.na(Todd_Tichenor)
colSums(is.na(Todd_Tichenor)) 
Todd_Tichenor = Todd_Tichenor[,colSums(is.na(Todd_Tichenor)) == 0] 
dim(Todd_Tichenor)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Todd_Tichenor = Todd_Tichenor[ , !(names(Todd_Tichenor) %in% drops)]
dim(Todd_Tichenor)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Todd_Tichenor_train = Todd_Tichenor[0:(0.8 * nrow(Todd_Tichenor)),]
dim(Todd_Tichenor_train)
prop.table(table(Todd_Tichenor_train$type))
Todd_Tichenor_test = Todd_Tichenor[(0.8*nrow(Todd_Tichenor)):nrow(Todd_Tichenor),]
dim(Todd_Tichenor_test)
prop.table(table(Todd_Tichenor_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Todd_Tichenor_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Todd_Tichenor_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Todd_Tichenor_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Todd_Tichenor_Strikes = subset(Todd_Tichenor, Todd_Tichenor$type == "S")
Todd_Tichenor_Balls = subset(Todd_Tichenor, Todd_Tichenor$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Todd_Tichenor_Strikes$AdjustedCall = ifelse((Todd_Tichenor_Strikes$plate_x < 0.833 & Todd_Tichenor_Strikes$plate_x > -0.833) & (Todd_Tichenor_Strikes$plate_z > Todd_Tichenor_Strikes$sz_bot & Todd_Tichenor_Strikes$plate_z < Todd_Tichenor_Strikes$sz_top), 'S', 'B')
table(Todd_Tichenor_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Todd_Tichenor_Balls$AdjustedCall = ifelse((Todd_Tichenor_Balls$plate_x > 0.833 | Todd_Tichenor_Balls$plate_x < -0.833)|(Todd_Tichenor_Balls$plate_z < Todd_Tichenor_Balls$sz_bot | Todd_Tichenor_Balls$plate_z > Todd_Tichenor_Balls$sz_top),'B','S')
table(Todd_Tichenor_Balls$AdjustedCall)

# Merge to create new dataset
Todd_Tichenor_AdjustedCalls = rbind(Todd_Tichenor_Strikes,Todd_Tichenor_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Todd_Tichenor)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Todd_Tichenor_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Todd_Tichenor_AdjustedCalls$AdjustedCall))







