# 4/18/18 : Giants vs __OPPONENT__ : Phil_Cuzzi

# 167 pitches were called strikes/balls

# The robot-ump called 42 of those pitches as called strikes & 125 as balls

# Phil_Cuzzi called 56 of those pitches as called strikes & 111 as balls 

# Accuracy: 90% 

Phil_Cuzzi <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Phil_Cuzzi_4:18:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Phil_Cuzzi)
names(Phil_Cuzzi)
is.na(Phil_Cuzzi)
colSums(is.na(Phil_Cuzzi)) 
Phil_Cuzzi = Phil_Cuzzi[,colSums(is.na(Phil_Cuzzi)) == 0] 
dim(Phil_Cuzzi)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Phil_Cuzzi = Phil_Cuzzi[ , !(names(Phil_Cuzzi) %in% drops)]
dim(Phil_Cuzzi)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Phil_Cuzzi_train = Phil_Cuzzi[0:(0.8 * nrow(Phil_Cuzzi)),]
dim(Phil_Cuzzi_train)
prop.table(table(Phil_Cuzzi_train$type))
Phil_Cuzzi_test = Phil_Cuzzi[(0.8*nrow(Phil_Cuzzi)):nrow(Phil_Cuzzi),]
dim(Phil_Cuzzi_test)
prop.table(table(Phil_Cuzzi_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Phil_Cuzzi_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Phil_Cuzzi_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Phil_Cuzzi_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Phil_Cuzzi_Strikes = subset(Phil_Cuzzi, Phil_Cuzzi$type == "S")
Phil_Cuzzi_Balls = subset(Phil_Cuzzi, Phil_Cuzzi$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Phil_Cuzzi_Strikes$AdjustedCall = ifelse((Phil_Cuzzi_Strikes$plate_x < 0.833 & Phil_Cuzzi_Strikes$plate_x > -0.833) & (Phil_Cuzzi_Strikes$plate_z > Phil_Cuzzi_Strikes$sz_bot & Phil_Cuzzi_Strikes$plate_z < Phil_Cuzzi_Strikes$sz_top), 'S', 'B')
table(Phil_Cuzzi_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Phil_Cuzzi_Balls$AdjustedCall = ifelse((Phil_Cuzzi_Balls$plate_x > 0.833 | Phil_Cuzzi_Balls$plate_x < -0.833)|(Phil_Cuzzi_Balls$plate_z < Phil_Cuzzi_Balls$sz_bot | Phil_Cuzzi_Balls$plate_z > Phil_Cuzzi_Balls$sz_top),'B','S')
table(Phil_Cuzzi_Balls$AdjustedCall)

# Merge to create new dataset
Phil_Cuzzi_AdjustedCalls = rbind(Phil_Cuzzi_Strikes,Phil_Cuzzi_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Phil_Cuzzi)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Phil_Cuzzi_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Phil_Cuzzi_AdjustedCalls$AdjustedCall))







