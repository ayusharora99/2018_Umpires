# 4/4/18 : Giants vs __OPPONENT__ : Lance Barksdale

# 153 pitches were called strikes/balls

# The robot-ump called 39 of those pitches as called strikes & 114 as balls

# Lance Barksdale called 44 of those pitches as called strikes & 109 as balls 

# Accuracy: 95% 

Lance_Barksdale <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Lance_Barksdale_4:4:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Lance_Barksdale)
names(Lance_Barksdale)
is.na(Lance_Barksdale)
colSums(is.na(Lance_Barksdale)) 
Lance_Barksdale = Lance_Barksdale[,colSums(is.na(Lance_Barksdale)) == 0] 
dim(Lance_Barksdale)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Lance_Barksdale = Lance_Barksdale[ , !(names(Lance_Barksdale) %in% drops)]
dim(Lance_Barksdale)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Lance_Barksdale_train = Lance_Barksdale[0:(0.8 * nrow(Lance_Barksdale)),]
dim(Lance_Barksdale_train)
prop.table(table(Lance_Barksdale_train$type))
Lance_Barksdale_test = Lance_Barksdale[(0.8*nrow(Lance_Barksdale)):nrow(Lance_Barksdale),]
dim(Lance_Barksdale_test)
prop.table(table(Lance_Barksdale_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Lance_Barksdale_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Lance_Barksdale_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Lance_Barksdale_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Lance_Barksdale_Strikes = subset(Lance_Barksdale, Lance_Barksdale$type == "S")
Lance_Barksdale_Balls = subset(Lance_Barksdale, Lance_Barksdale$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Lance_Barksdale_Strikes$AdjustedCall = ifelse((Lance_Barksdale_Strikes$plate_x < 0.833 & Lance_Barksdale_Strikes$plate_x > -0.833) & (Lance_Barksdale_Strikes$plate_z > Lance_Barksdale_Strikes$sz_bot & Lance_Barksdale_Strikes$plate_z < Lance_Barksdale_Strikes$sz_top), 'S', 'B')
table(Lance_Barksdale_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Lance_Barksdale_Balls$AdjustedCall = ifelse((Lance_Barksdale_Balls$plate_x > 0.833 | Lance_Barksdale_Balls$plate_x < -0.833)|(Lance_Barksdale_Balls$plate_z < Lance_Barksdale_Balls$sz_bot | Lance_Barksdale_Balls$plate_z > Lance_Barksdale_Balls$sz_top),'B','S')
table(Lance_Barksdale_Balls$AdjustedCall)

# Merge to create new dataset
Lance_Barksdale_AdjustedCalls = rbind(Lance_Barksdale_Strikes,Lance_Barksdale_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Lance_Barksdale)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Lance_Barksdale_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Lance_Barksdale_AdjustedCalls$AdjustedCall))







