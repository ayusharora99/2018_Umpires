# 4/12/18 : Giants vs __OPPONENT__ : Mike Winters

# 158 pitches were called strikes/balls

# The robot-ump called 42 of those pitches as called strikes & 116 as balls

# Mike_Winters called 56 of those pitches as called strikes & 102 as balls 

# Accuracy: 89% 

Mike_Winters <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mike_Winters_4:12:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Mike_Winters)
names(Mike_Winters)
is.na(Mike_Winters)
colSums(is.na(Mike_Winters)) 
Mike_Winters = Mike_Winters[,colSums(is.na(Mike_Winters)) == 0] 
dim(Mike_Winters)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mike_Winters = Mike_Winters[ , !(names(Mike_Winters) %in% drops)]
dim(Mike_Winters)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Mike_Winters_train = Mike_Winters[0:(0.8 * nrow(Mike_Winters)),]
dim(Mike_Winters_train)
prop.table(table(Mike_Winters_train$type))
Mike_Winters_test = Mike_Winters[(0.8*nrow(Mike_Winters)):nrow(Mike_Winters),]
dim(Mike_Winters_test)
prop.table(table(Mike_Winters_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mike_Winters_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Mike_Winters_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Mike_Winters_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Mike_Winters_Strikes = subset(Mike_Winters, Mike_Winters$type == "S")
Mike_Winters_Balls = subset(Mike_Winters, Mike_Winters$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mike_Winters_Strikes$AdjustedCall = ifelse((Mike_Winters_Strikes$plate_x < 0.833 & Mike_Winters_Strikes$plate_x > -0.833) & (Mike_Winters_Strikes$plate_z > Mike_Winters_Strikes$sz_bot & Mike_Winters_Strikes$plate_z < Mike_Winters_Strikes$sz_top), 'S', 'B')
table(Mike_Winters_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mike_Winters_Balls$AdjustedCall = ifelse((Mike_Winters_Balls$plate_x > 0.833 | Mike_Winters_Balls$plate_x < -0.833)|(Mike_Winters_Balls$plate_z < Mike_Winters_Balls$sz_bot | Mike_Winters_Balls$plate_z > Mike_Winters_Balls$sz_top),'B','S')
table(Mike_Winters_Balls$AdjustedCall)

# Merge to create new dataset
Mike_Winters_AdjustedCalls = rbind(Mike_Winters_Strikes,Mike_Winters_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Mike_Winters)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Mike_Winters_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Mike_Winters_AdjustedCalls$AdjustedCall))







