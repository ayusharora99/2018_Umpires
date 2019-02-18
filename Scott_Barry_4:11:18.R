# 4/11/18 : Giants vs __OPPONENT__ : Scott_Barry

# 161 pitches were called strikes/balls

# The robot-ump called 49 of those pitches as called strikes & 112 as balls

# Scott_Barry called 61 of those pitches as called strikes & 100 as balls 

# Accuracy: 93% 

Scott_Barry <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Scott_Barry_4:11:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))
library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Scott_Barry)
names(Scott_Barry)
is.na(Scott_Barry)
colSums(is.na(Scott_Barry)) 
Scott_Barry = Scott_Barry[,colSums(is.na(Scott_Barry)) == 0] 
dim(Scott_Barry)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Scott_Barry = Scott_Barry[ , !(names(Scott_Barry) %in% drops)]
dim(Scott_Barry)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Scott_Barry_train = Scott_Barry[0:(0.8 * nrow(Scott_Barry)),]
dim(Scott_Barry_train)
prop.table(table(Scott_Barry_train$type))
Scott_Barry_test = Scott_Barry[(0.8*nrow(Scott_Barry)):nrow(Scott_Barry),]
dim(Scott_Barry_test)
prop.table(table(Scott_Barry_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Scott_Barry_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Scott_Barry_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Scott_Barry_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Scott_Barry_Strikes = subset(Scott_Barry, Scott_Barry$type == "S")
Scott_Barry_Balls = subset(Scott_Barry, Scott_Barry$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Scott_Barry_Strikes$AdjustedCall = ifelse((Scott_Barry_Strikes$plate_x < 0.833 & Scott_Barry_Strikes$plate_x > -0.833) & (Scott_Barry_Strikes$plate_z > Scott_Barry_Strikes$sz_bot & Scott_Barry_Strikes$plate_z < Scott_Barry_Strikes$sz_top), 'S', 'B')
table(Scott_Barry_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Scott_Barry_Balls$AdjustedCall = ifelse((Scott_Barry_Balls$plate_x > 0.833 | Scott_Barry_Balls$plate_x < -0.833)|(Scott_Barry_Balls$plate_z < Scott_Barry_Balls$sz_bot | Scott_Barry_Balls$plate_z > Scott_Barry_Balls$sz_top),'B','S')
table(Scott_Barry_Balls$AdjustedCall)

# Merge to create new dataset
Scott_Barry_AdjustedCalls = rbind(Scott_Barry_Strikes,Scott_Barry_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Scott_Barry)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Scott_Barry_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Scott_Barry_AdjustedCalls$AdjustedCall))







