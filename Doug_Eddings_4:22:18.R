# __DATE__ : Giants vs __OPPONENT__ : Doug_Eddings

# 182 pitches were called strikes/balls

# The robot-ump called 58 of those pitches as called strikes & 124 as balls

# Doug Eddings called 71 of those pitches as called strikes & 111 as balls 

# Accuracy: 93% 

Doug_Eddings <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Doug_Eddings_4:22:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Doug_Eddings)
names(Doug_Eddings)
is.na(Doug_Eddings)
colSums(is.na(Doug_Eddings)) 
Doug_Eddings = Doug_Eddings[,colSums(is.na(Doug_Eddings)) == 0] 
dim(Doug_Eddings)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Doug_Eddings = Doug_Eddings[ , !(names(Doug_Eddings) %in% drops)]
dim(Doug_Eddings)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Doug_Eddings_train = Doug_Eddings[0:(0.8 * nrow(Doug_Eddings)),]
dim(Doug_Eddings_train)
prop.table(table(Doug_Eddings_train$type))
Doug_Eddings_test = Doug_Eddings[(0.8*nrow(Doug_Eddings)):nrow(Doug_Eddings),]
dim(Doug_Eddings_test)
prop.table(table(Doug_Eddings_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Doug_Eddings_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Doug_Eddings_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Doug_Eddings_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Doug_Eddings_Strikes = subset(Doug_Eddings, Doug_Eddings$type == "S")
Doug_Eddings_Balls = subset(Doug_Eddings, Doug_Eddings$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Doug_Eddings_Strikes$AdjustedCall = ifelse((Doug_Eddings_Strikes$plate_x < 0.833 & Doug_Eddings_Strikes$plate_x > -0.833) & (Doug_Eddings_Strikes$plate_z > Doug_Eddings_Strikes$sz_bot & Doug_Eddings_Strikes$plate_z < Doug_Eddings_Strikes$sz_top), 'S', 'B')
table(Doug_Eddings_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Doug_Eddings_Balls$AdjustedCall = ifelse((Doug_Eddings_Balls$plate_x > 0.833 | Doug_Eddings_Balls$plate_x < -0.833)|(Doug_Eddings_Balls$plate_z < Doug_Eddings_Balls$sz_bot | Doug_Eddings_Balls$plate_z > Doug_Eddings_Balls$sz_top),'B','S')
table(Doug_Eddings_Balls$AdjustedCall)

# Merge to create new dataset
Doug_Eddings_AdjustedCalls = rbind(Doug_Eddings_Strikes,Doug_Eddings_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Doug_Eddings)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Doug_Eddings_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Doug_Eddings_AdjustedCalls$AdjustedCall))







