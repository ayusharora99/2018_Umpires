# 6/16/18 : Giants vs __OPPONENT__ : Dan Bellino

# 138 pitches were called strikes/balls

# The robot-ump called 40 of those pitches as called strikes & 98 as balls

# __UMPIRE__ called 44 of those pitches as called strikes & 94 as balls 

# Accuracy: 90%

Dan_Bellino <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Dan_Bellino_6:16:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Dan_Bellino)
names(Dan_Bellino)
is.na(Dan_Bellino)
colSums(is.na(Dan_Bellino)) 
Dan_Bellino = Dan_Bellino[,colSums(is.na(Dan_Bellino)) == 0] 
dim(Dan_Bellino)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Dan_Bellino = Dan_Bellino[ , !(names(Dan_Bellino) %in% drops)]
dim(Dan_Bellino)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Dan_Bellino_train = Dan_Bellino[0:(0.8 * nrow(Dan_Bellino)),]
dim(Dan_Bellino_train)
prop.table(table(Dan_Bellino_train$type))
Dan_Bellino_test = Dan_Bellino[(0.8*nrow(Dan_Bellino)):nrow(Dan_Bellino),]
dim(Dan_Bellino_test)
prop.table(table(Dan_Bellino_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Dan_Bellino_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Dan_Bellino_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Dan_Bellino_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Dan_Bellino_Strikes = subset(Dan_Bellino, Dan_Bellino$type == "S")
Dan_Bellino_Balls = subset(Dan_Bellino, Dan_Bellino$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Dan_Bellino_Strikes$AdjustedCall = ifelse((Dan_Bellino_Strikes$plate_x < 0.833 & Dan_Bellino_Strikes$plate_x > -0.833) & (Dan_Bellino_Strikes$plate_z > Dan_Bellino_Strikes$sz_bot & Dan_Bellino_Strikes$plate_z < Dan_Bellino_Strikes$sz_top), 'S', 'B')
table(Dan_Bellino_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Dan_Bellino_Balls$AdjustedCall = ifelse((Dan_Bellino_Balls$plate_x > 0.833 | Dan_Bellino_Balls$plate_x < -0.833)|(Dan_Bellino_Balls$plate_z < Dan_Bellino_Balls$sz_bot | Dan_Bellino_Balls$plate_z > Dan_Bellino_Balls$sz_top),'B','S')
table(Dan_Bellino_Balls$AdjustedCall)

# Merge to create new dataset
Dan_Bellino_AdjustedCalls = rbind(Dan_Bellino_Strikes,Dan_Bellino_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Dan_Bellino)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Dan_Bellino_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Dan_Bellino_AdjustedCalls$AdjustedCall))







