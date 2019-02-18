# 6/2/18 : Giants vs __OPPONENT__ : Jerry Meals

# 102 pitches were called strikes/balls

# The robot-ump called 36 of those pitches as called strikes & 65 as balls

# Jerry Meals called 40 of those pitches as called strikes & 62 as balls 

# Accuracy: 90%

Jerry_Meals <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jerry_Meals_6:2:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Jerry_Meals)
names(Jerry_Meals)
is.na(Jerry_Meals)
colSums(is.na(Jerry_Meals)) 
Jerry_Meals = Jerry_Meals[,colSums(is.na(Jerry_Meals)) == 0] 
dim(Jerry_Meals)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jerry_Meals = Jerry_Meals[ , !(names(Jerry_Meals) %in% drops)]
dim(Jerry_Meals)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Jerry_Meals_train = Jerry_Meals[0:(0.8 * nrow(Jerry_Meals)),]
dim(Jerry_Meals_train)
prop.table(table(Jerry_Meals_train$type))
Jerry_Meals_test = Jerry_Meals[(0.8*nrow(Jerry_Meals)):nrow(Jerry_Meals),]
dim(Jerry_Meals_test)
prop.table(table(Jerry_Meals_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jerry_Meals_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Jerry_Meals_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Jerry_Meals_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Jerry_Meals_Strikes = subset(Jerry_Meals, Jerry_Meals$type == "S")
Jerry_Meals_Balls = subset(Jerry_Meals, Jerry_Meals$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jerry_Meals_Strikes$AdjustedCall = ifelse((Jerry_Meals_Strikes$plate_x < 0.833 & Jerry_Meals_Strikes$plate_x > -0.833) & (Jerry_Meals_Strikes$plate_z > Jerry_Meals_Strikes$sz_bot & Jerry_Meals_Strikes$plate_z < Jerry_Meals_Strikes$sz_top), 'S', 'B')
table(Jerry_Meals_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jerry_Meals_Balls$AdjustedCall = ifelse((Jerry_Meals_Balls$plate_x > 0.833 | Jerry_Meals_Balls$plate_x < -0.833)|(Jerry_Meals_Balls$plate_z < Jerry_Meals_Balls$sz_bot | Jerry_Meals_Balls$plate_z > Jerry_Meals_Balls$sz_top),'B','S')
table(Jerry_Meals_Balls$AdjustedCall)

# Merge to create new dataset
Jerry_Meals_AdjustedCalls = rbind(Jerry_Meals_Strikes,Jerry_Meals_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Jerry_Meals)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Jerry_Meals_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Jerry_Meals_AdjustedCalls$AdjustedCall))







