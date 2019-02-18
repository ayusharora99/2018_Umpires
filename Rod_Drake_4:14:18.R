# 4/14/18 : Giants vs __OPPONENT__ : Rod_Drake

# 159 pitches were called strikes/balls

# The robot-ump called 46 of those pitches as called strikes & 113 as balls

# Rod_Drake called 55 of those pitches as called strikes & 104 as balls 

# Accuracy: 89% 

Rod_Drake <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Rod_Drake_4:14:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Rod_Drake)
names(Rod_Drake)
is.na(Rod_Drake)
colSums(is.na(Rod_Drake)) 
Rod_Drake = Rod_Drake[,colSums(is.na(Rod_Drake)) == 0] 
dim(Rod_Drake)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Rod_Drake = Rod_Drake[ , !(names(Rod_Drake) %in% drops)]
dim(Rod_Drake)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Rod_Drake_train = Rod_Drake[0:(0.8 * nrow(Rod_Drake)),]
dim(Rod_Drake_train)
prop.table(table(Rod_Drake_train$type))
Rod_Drake_test = Rod_Drake[(0.8*nrow(Rod_Drake)):nrow(Rod_Drake),]
dim(Rod_Drake_test)
prop.table(table(Rod_Drake_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Rod_Drake_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Rod_Drake_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Rod_Drake_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Rod_Drake_Strikes = subset(Rod_Drake, Rod_Drake$type == "S")
Rod_Drake_Balls = subset(Rod_Drake, Rod_Drake$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Rod_Drake_Strikes$AdjustedCall = ifelse((Rod_Drake_Strikes$plate_x < 0.833 & Rod_Drake_Strikes$plate_x > -0.833) & (Rod_Drake_Strikes$plate_z > Rod_Drake_Strikes$sz_bot & Rod_Drake_Strikes$plate_z < Rod_Drake_Strikes$sz_top), 'S', 'B')
table(Rod_Drake_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Rod_Drake_Balls$AdjustedCall = ifelse((Rod_Drake_Balls$plate_x > 0.833 | Rod_Drake_Balls$plate_x < -0.833)|(Rod_Drake_Balls$plate_z < Rod_Drake_Balls$sz_bot | Rod_Drake_Balls$plate_z > Rod_Drake_Balls$sz_top),'B','S')
table(Rod_Drake_Balls$AdjustedCall)

# Merge to create new dataset
Rod_Drake_AdjustedCalls = rbind(Rod_Drake_Strikes,Rod_Drake_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Rod_Drake)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Rod_Drake_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Rod_Drake_AdjustedCalls$AdjustedCall))







