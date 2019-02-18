# 4/30/18 : Giants vs __OPPONENT__ : Jerry_Lane

# 171 pitches were called strikes/balls

# The robot-ump called 45 of those pitches as called strikes & 126 as balls

# Jerry_Lane called 49 of those pitches as called strikes & 122 as balls 

# Accuracy: 90% 

Jerry_Lane <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jerry_Lane_4:30:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Jerry_Lane)
names(Jerry_Lane)
is.na(Jerry_Lane)
colSums(is.na(Jerry_Lane)) 
Jerry_Lane = Jerry_Lane[,colSums(is.na(Jerry_Lane)) == 0] 
dim(Jerry_Lane)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jerry_Lane = Jerry_Lane[ , !(names(Jerry_Lane) %in% drops)]
dim(Jerry_Lane)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Jerry_Lane_train = Jerry_Lane[0:(0.8 * nrow(Jerry_Lane)),]
dim(Jerry_Lane_train)
prop.table(table(Jerry_Lane_train$type))
Jerry_Lane_test = Jerry_Lane[(0.8*nrow(Jerry_Lane)):nrow(Jerry_Lane),]
dim(Jerry_Lane_test)
prop.table(table(Jerry_Lane_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jerry_Lane_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Jerry_Lane_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Jerry_Lane_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Jerry_Lane_Strikes = subset(Jerry_Lane, Jerry_Lane$type == "S")
Jerry_Lane_Balls = subset(Jerry_Lane, Jerry_Lane$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jerry_Lane_Strikes$AdjustedCall = ifelse((Jerry_Lane_Strikes$plate_x < 0.833 & Jerry_Lane_Strikes$plate_x > -0.833) & (Jerry_Lane_Strikes$plate_z > Jerry_Lane_Strikes$sz_bot & Jerry_Lane_Strikes$plate_z < Jerry_Lane_Strikes$sz_top), 'S', 'B')
table(Jerry_Lane_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jerry_Lane_Balls$AdjustedCall = ifelse((Jerry_Lane_Balls$plate_x > 0.833 | Jerry_Lane_Balls$plate_x < -0.833)|(Jerry_Lane_Balls$plate_z < Jerry_Lane_Balls$sz_bot | Jerry_Lane_Balls$plate_z > Jerry_Lane_Balls$sz_top),'B','S')
table(Jerry_Lane_Balls$AdjustedCall)

# Merge to create new dataset
Jerry_Lane_AdjustedCalls = rbind(Jerry_Lane_Strikes,Jerry_Lane_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Jerry_Lane)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Jerry_Lane_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Jerry_Lane_AdjustedCalls$AdjustedCall))







