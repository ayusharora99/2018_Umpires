# 7/9/18 : Giants vs Cubs : Jeff Nelson

# 152 pitches were called strikes/balls

# The robot-ump called 43 of those pitches as called strikes & 109 as balls

# Jeff Nelson called 54 of those pitches as called strikes & 98 as balls 

# Accuracy: 90%

Jeff_Nelson <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jeff_Nelson_7:9:19.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Jeff_Nelson)
names(Jeff_Nelosn)
is.na(Jeff_Nelson)
colSums(is.na(Jeff_Nelson)) 
Jeff_Nelson = Jeff_Nelson[,colSums(is.na(Jeff_Nelson)) == 0] 
dim(Jeff_Nelson)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jeff_Nelson = Jeff_Nelson[ , !(names(Jeff_Nelson) %in% drops)]
dim(Jeff_Nelson)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Jeff_Nelson_train = Jeff_Nelson[0:(0.8 * nrow(Jeff_Nelson)),]
dim(Jeff_Nelson_train)
prop.table(table(Jeff_Nelson_train$type))
Jeff_Nelson_test = Jeff_Nelson[(0.8*nrow(Jeff_Nelson)):nrow(Jeff_Nelson),]
dim(Jeff_Nelson_test)
prop.table(table(Jeff_Nelson_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jeff_Nelson_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Jeff_Nelson_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Jeff_Nelson_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Jeff_Nelson_Strikes = subset(Jeff_Nelson, Jeff_Nelson$type == "S")
Jeff_Nelson_Balls = subset(Jeff_Nelson, Jeff_Nelson$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jeff_Nelson_Strikes$AdjustedCall = ifelse((Jeff_Nelson_Strikes$plate_x < 0.833 & Jeff_Nelson_Strikes$plate_x > -0.833) & (Jeff_Nelson_Strikes$plate_z > Jeff_Nelson_Strikes$sz_bot & Jeff_Nelson_Strikes$plate_z < Jeff_Nelson_Strikes$sz_top), 'S', 'B')
table(Jeff_Nelson_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jeff_Nelson_Balls$AdjustedCall = ifelse((Jeff_Nelson_Balls$plate_x > 0.833 | Jeff_Nelson_Balls$plate_x < -0.833)|(Jeff_Nelson_Balls$plate_z < Jeff_Nelson_Balls$sz_bot | Jeff_Nelson_Balls$plate_z > Jeff_Nelson_Balls$sz_top),'B','S')
table(Jeff_Nelson_Balls$AdjustedCall)

# Merge to create new dataset
Jeff_Nelson_AdjustedCalls = rbind(Jeff_Nelson_Strikes,Jeff_Nelson_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Jeff_Nelson)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Jeff_Nelson_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Jeff_Nelson_AdjustedCalls$AdjustedCall))







