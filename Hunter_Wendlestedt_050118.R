# 05/01/2018 : Giants vs Padres : Hunter Wendelstedt
# 144 pitches were called strikes/balls

# The robot-ump called 42 of those pitches as called strikes & 102 as balls

# Hunter Wendelstedt called 52 of those pitches as called strikes & 92 as balls 

# Accuracy: 90%
Hunter_Wendelstedt_050118 <- read.csv("C:/Users/Owner/Desktop/2018 Giants Umpires/Hunter_Wendelstedt_050118.csv", header=TRUE)
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Hunter_Wendelstedt_050118)
names(Hunter_Wendelstedt_050118)
is.na(Hunter_Wendelstedt_050118)
colSums(is.na(Hunter_Wendelstedt_050118)) 
Hunter_Wendelstedt_050118 = Hunter_Wendelstedt_050118[,colSums(is.na(Hunter_Wendelstedt_050118)) == 0] 
dim(Hunter_Wendelstedt_050118)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Hunter_Wendelstedt_050118 = Hunter_Wendelstedt_050118[ , !(names(Hunter_Wendelstedt_050118) %in% drops)]
dim(Hunter_Wendelstedt_050118)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Hunter_Wendelstedt_050118_train = Hunter_Wendelstedt_050118[0:(0.8 * nrow(Hunter_Wendelstedt_050118)),]
dim(Hunter_Wendelstedt_050118_train)
prop.table(table(Hunter_Wendelstedt_050118_train$type))
Hunter_Wendelstedt_050118_test = Hunter_Wendelstedt_050118[(0.8*nrow(Hunter_Wendelstedt_050118)):nrow(Hunter_Wendelstedt_050118),]
dim(Hunter_Wendelstedt_050118_test)
prop.table(table(Hunter_Wendelstedt_050118_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Hunter_Wendelstedt_050118_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Hunter_Wendelstedt_050118_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Hunter_Wendelstedt_050118_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Hunter_Wendelstedt_050118_Strikes = subset(Hunter_Wendelstedt_050118, Hunter_Wendelstedt_050118$type == "S")
Hunter_Wendelstedt_050118_Balls = subset(Hunter_Wendelstedt_050118, Hunter_Wendelstedt_050118$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Hunter_Wendelstedt_050118_Strikes$AdjustedCall = ifelse((Hunter_Wendelstedt_050118_Strikes$plate_x < 0.833 & Hunter_Wendelstedt_050118_Strikes$plate_x > -0.833) & (Hunter_Wendelstedt_050118_Strikes$plate_z > Hunter_Wendelstedt_050118_Strikes$sz_bot & Hunter_Wendelstedt_050118_Strikes$plate_z < Hunter_Wendelstedt_050118_Strikes$sz_top), 'S', 'B')
table(Hunter_Wendelstedt_050118_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Hunter_Wendelstedt_050118_Balls$AdjustedCall = ifelse((Hunter_Wendelstedt_050118_Balls$plate_x > 0.833 | Hunter_Wendelstedt_050118_Balls$plate_x < -0.833)|(Hunter_Wendelstedt_050118_Balls$plate_z < Hunter_Wendelstedt_050118_Balls$sz_bot | Hunter_Wendelstedt_050118_Balls$plate_z > Hunter_Wendelstedt_050118_Balls$sz_top),'B','S')
table(Hunter_Wendelstedt_050118_Balls$AdjustedCall)

# Merge to create new dataset
Hunter_Wendelstedt_050118_AdjustedCalls = rbind(Hunter_Wendelstedt_050118_Strikes,Hunter_Wendelstedt_050118_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Hunter_Wendelstedt_050118)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Hunter_Wendelstedt_050118_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Hunter_Wendelstedt_050118_AdjustedCalls$AdjustedCall))

