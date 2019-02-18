# 4/15/18 : Giants vs __OPPONENT__ : Mike_Muchlinkski

# 165 pitches were called strikes/balls

# The robot-ump called 47 of those pitches as called strikes & 118 as balls

# Mike_Muchlinkski called 57 of those pitches as called strikes & 108 as balls 

# Accuracy: 93% 

Mike_Muchlinkski <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mike_Muchlinkski_4:1:5:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Mike_Muchlinkski)
names(Mike_Muchlinkski)
is.na(Mike_Muchlinkski)
colSums(is.na(Mike_Muchlinkski)) 
Mike_Muchlinkski = Mike_Muchlinkski[,colSums(is.na(Mike_Muchlinkski)) == 0] 
dim(Mike_Muchlinkski)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mike_Muchlinkski = Mike_Muchlinkski[ , !(names(Mike_Muchlinkski) %in% drops)]
dim(Mike_Muchlinkski)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Mike_Muchlinkski_train = Mike_Muchlinkski[0:(0.8 * nrow(Mike_Muchlinkski)),]
dim(Mike_Muchlinkski_train)
prop.table(table(Mike_Muchlinkski_train$type))
Mike_Muchlinkski_test = Mike_Muchlinkski[(0.8*nrow(Mike_Muchlinkski)):nrow(Mike_Muchlinkski),]
dim(Mike_Muchlinkski_test)
prop.table(table(Mike_Muchlinkski_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mike_Muchlinkski_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Mike_Muchlinkski_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Mike_Muchlinkski_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Mike_Muchlinkski_Strikes = subset(Mike_Muchlinkski, Mike_Muchlinkski$type == "S")
Mike_Muchlinkski_Balls = subset(Mike_Muchlinkski, Mike_Muchlinkski$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mike_Muchlinkski_Strikes$AdjustedCall = ifelse((Mike_Muchlinkski_Strikes$plate_x < 0.833 & Mike_Muchlinkski_Strikes$plate_x > -0.833) & (Mike_Muchlinkski_Strikes$plate_z > Mike_Muchlinkski_Strikes$sz_bot & Mike_Muchlinkski_Strikes$plate_z < Mike_Muchlinkski_Strikes$sz_top), 'S', 'B')
table(Mike_Muchlinkski_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mike_Muchlinkski_Balls$AdjustedCall = ifelse((Mike_Muchlinkski_Balls$plate_x > 0.833 | Mike_Muchlinkski_Balls$plate_x < -0.833)|(Mike_Muchlinkski_Balls$plate_z < Mike_Muchlinkski_Balls$sz_bot | Mike_Muchlinkski_Balls$plate_z > Mike_Muchlinkski_Balls$sz_top),'B','S')
table(Mike_Muchlinkski_Balls$AdjustedCall)

# Merge to create new dataset
Mike_Muchlinkski_AdjustedCalls = rbind(Mike_Muchlinkski_Strikes,Mike_Muchlinkski_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Mike_Muchlinkski)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Mike_Muchlinkski_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Mike_Muchlinkski_AdjustedCalls$AdjustedCall))







