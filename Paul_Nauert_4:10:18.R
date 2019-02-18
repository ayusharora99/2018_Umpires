# 4/10/18 : Giants vs __OPPONENT__ : Paul_Nauert

# 183 pitches were called strikes/balls

# The robot-ump called 39 of those pitches as called strikes & 143 as balls

# Paul_Nauert called 61 of those pitches as called strikes & 122 as balls 

# Accuracy: 87% 

Paul_Nauert <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Paul_Nauert_4:10:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Paul_Nauert)
names(Paul_Nauert)
is.na(Paul_Nauert)
colSums(is.na(Paul_Nauert)) 
Paul_Nauert = Paul_Nauert[,colSums(is.na(Paul_Nauert)) == 0] 
dim(Paul_Nauert)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Paul_Nauert = Paul_Nauert[ , !(names(Paul_Nauert) %in% drops)]
dim(Paul_Nauert)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Paul_Nauert_train = Paul_Nauert[0:(0.8 * nrow(Paul_Nauert)),]
dim(Paul_Nauert_train)
prop.table(table(Paul_Nauert_train$type))
Paul_Nauert_test = Paul_Nauert[(0.8*nrow(Paul_Nauert)):nrow(Paul_Nauert),]
dim(Paul_Nauert_test)
prop.table(table(Paul_Nauert_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Paul_Nauert_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Paul_Nauert_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Paul_Nauert_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Paul_Nauert_Strikes = subset(Paul_Nauert, Paul_Nauert$type == "S")
Paul_Nauert_Balls = subset(Paul_Nauert, Paul_Nauert$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Paul_Nauert_Strikes$AdjustedCall = ifelse((Paul_Nauert_Strikes$plate_x < 0.833 & Paul_Nauert_Strikes$plate_x > -0.833) & (Paul_Nauert_Strikes$plate_z > Paul_Nauert_Strikes$sz_bot & Paul_Nauert_Strikes$plate_z < Paul_Nauert_Strikes$sz_top), 'S', 'B')
table(Paul_Nauert_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Paul_Nauert_Balls$AdjustedCall = ifelse((Paul_Nauert_Balls$plate_x > 0.833 | Paul_Nauert_Balls$plate_x < -0.833)|(Paul_Nauert_Balls$plate_z < Paul_Nauert_Balls$sz_bot | Paul_Nauert_Balls$plate_z > Paul_Nauert_Balls$sz_top),'B','S')
table(Paul_Nauert_Balls$AdjustedCall)

# Merge to create new dataset
Paul_Nauert_AdjustedCalls = rbind(Paul_Nauert_Strikes,Paul_Nauert_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Paul_Nauert)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Paul_Nauert_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Paul_Nauert_AdjustedCalls$AdjustedCall))







