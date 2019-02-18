# 7/1/18 : Giants vs __OPPONENT__ : Adrian Johnson

# 158 pitches were called strikes/balls

# The robot-ump called 47 of those pitches as called strikes & 111 as balls

# Adrian Johnson called 52 of those pitches as called strikes & 106 as balls 

# Accuracy: 89%

Adrian_Johnson <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Adrian_Johnson_7:1:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Adrian_Johnson)
names(Adrian_Johnson)
is.na(Adrian_Johnson)
colSums(is.na(Adrian_Johnson)) 
Adrian_Johnson = Adrian_Johnson[,colSums(is.na(Adrian_Johnson)) == 0] 
dim(Adrian_Johnson)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Adrian_Johnson = Adrian_Johnson[ , !(names(Adrian_Johnson) %in% drops)]
dim(Adrian_Johnson)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Adrian_Johnson_train = Adrian_Johnson[0:(0.8 * nrow(Adrian_Johnson)),]
dim(Adrian_Johnson_train)
prop.table(table(Adrian_Johnson_train$type))
Adrian_Johnson_test = Adrian_Johnson[(0.8*nrow(Adrian_Johnson)):nrow(Adrian_Johnson),]
dim(Adrian_Johnson_test)
prop.table(table(Adrian_Johnson_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Adrian_Johnson_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Adrian_Johnson_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Adrian_Johnson_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Adrian_Johnson_Strikes = subset(Adrian_Johnson, Adrian_Johnson$type == "S")
Adrian_Johnson_Balls = subset(Adrian_Johnson, Adrian_Johnson$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Adrian_Johnson_Strikes$AdjustedCall = ifelse((Adrian_Johnson_Strikes$plate_x < 0.833 & Adrian_Johnson_Strikes$plate_x > -0.833) & (Adrian_Johnson_Strikes$plate_z > Adrian_Johnson_Strikes$sz_bot & Adrian_Johnson_Strikes$plate_z < Adrian_Johnson_Strikes$sz_top), 'S', 'B')
table(Adrian_Johnson_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Adrian_Johnson_Balls$AdjustedCall = ifelse((Adrian_Johnson_Balls$plate_x > 0.833 | Adrian_Johnson_Balls$plate_x < -0.833)|(Adrian_Johnson_Balls$plate_z < Adrian_Johnson_Balls$sz_bot | Adrian_Johnson_Balls$plate_z > Adrian_Johnson_Balls$sz_top),'B','S')
table(Adrian_Johnson_Balls$AdjustedCall)

# Merge to create new dataset
Adrian_Johnson_AdjustedCalls = rbind(Adrian_Johnson_Strikes,Adrian_Johnson_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Adrian_Johnson)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Adrian_Johnson_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Adrian_Johnson_AdjustedCalls$AdjustedCall))







