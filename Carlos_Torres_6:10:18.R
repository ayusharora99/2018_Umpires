# 6/10/18 : Giants vs __OPPONENT__ : Carlos Torres

# 135 pitches were called strikes/balls

# The robot-ump called 34 of those pitches as called strikes & 101 as balls

# Carlose Torres called 41 of those pitches as called strikes & 94 as balls 

# Accuracy: 95%

Carlos_Torres <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Carlos_Torres_6:10:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Carlos_Torres)
names(Carlos_Torres)
is.na(Carlos_Torres)
colSums(is.na(Carlos_Torres)) 
Carlos_Torres = Carlos_Torres[,colSums(is.na(Carlos_Torres)) == 0] 
dim(Carlos_Torres)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Carlos_Torres = Carlos_Torres[ , !(names(Carlos_Torres) %in% drops)]
dim(Carlos_Torres)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Carlos_Torres_train = Carlos_Torres[0:(0.8 * nrow(Carlos_Torres)),]
dim(Carlos_Torres_train)
prop.table(table(Carlos_Torres_train$type))
Carlos_Torres_test = Carlos_Torres[(0.8*nrow(Carlos_Torres)):nrow(Carlos_Torres),]
dim(Carlos_Torres_test)
prop.table(table(Carlos_Torres_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Carlos_Torres_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Carlos_Torres_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Carlos_Torres_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Carlos_Torres_Strikes = subset(Carlos_Torres, Carlos_Torres$type == "S")
Carlos_Torres_Balls = subset(Carlos_Torres, Carlos_Torres$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Carlos_Torres_Strikes$AdjustedCall = ifelse((Carlos_Torres_Strikes$plate_x < 0.833 & Carlos_Torres_Strikes$plate_x > -0.833) & (Carlos_Torres_Strikes$plate_z > Carlos_Torres_Strikes$sz_bot & Carlos_Torres_Strikes$plate_z < Carlos_Torres_Strikes$sz_top), 'S', 'B')
table(Carlos_Torres_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Carlos_Torres_Balls$AdjustedCall = ifelse((Carlos_Torres_Balls$plate_x > 0.833 | Carlos_Torres_Balls$plate_x < -0.833)|(Carlos_Torres_Balls$plate_z < Carlos_Torres_Balls$sz_bot | Carlos_Torres_Balls$plate_z > Carlos_Torres_Balls$sz_top),'B','S')
table(Carlos_Torres_Balls$AdjustedCall)

# Merge to create new dataset
Carlos_Torres_AdjustedCalls = rbind(Carlos_Torres_Strikes,Carlos_Torres_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Carlos_Torres)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Carlos_Torres_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Carlos_Torres_AdjustedCalls$AdjustedCall))







