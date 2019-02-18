# 4/29/18 : Giants vs __OPPONENT__ : Dan Iassogna

# 352 pitches were called strikes/balls

# The robot-ump called 105 of those pitches as called strikes & 247 as balls

# Dan Iassogna called 121 of those pitches as called strikes & 231 as balls 

# Accuracy: 92% 

Dan_Iassogna <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Dan_Iassogna_4:29:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Dan_Iassogna)
names(Dan_Iassogna)
is.na(Dan_Iassogna)
colSums(is.na(Dan_Iassogna)) 
Dan_Iassogna = Dan_Iassogna[,colSums(is.na(Dan_Iassogna)) == 0] 
dim(Dan_Iassogna)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Dan_Iassogna = Dan_Iassogna[ , !(names(Dan_Iassogna) %in% drops)]
dim(Dan_Iassogna)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Dan_Iassogna_train = Dan_Iassogna[0:(0.8 * nrow(Dan_Iassogna)),]
dim(Dan_Iassogna_train)
prop.table(table(Dan_Iassogna_train$type))
Dan_Iassogna_test = Dan_Iassogna[(0.8*nrow(Dan_Iassogna)):nrow(Dan_Iassogna),]
dim(Dan_Iassogna_test)
prop.table(table(Dan_Iassogna_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Dan_Iassogna_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Dan_Iassogna_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Dan_Iassogna_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Dan_Iassogna_Strikes = subset(Dan_Iassogna, Dan_Iassogna$type == "S")
Dan_Iassogna_Balls = subset(Dan_Iassogna, Dan_Iassogna$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Dan_Iassogna_Strikes$AdjustedCall = ifelse((Dan_Iassogna_Strikes$plate_x < 0.833 & Dan_Iassogna_Strikes$plate_x > -0.833) & (Dan_Iassogna_Strikes$plate_z > Dan_Iassogna_Strikes$sz_bot & Dan_Iassogna_Strikes$plate_z < Dan_Iassogna_Strikes$sz_top), 'S', 'B')
table(Dan_Iassogna_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Dan_Iassogna_Balls$AdjustedCall = ifelse((Dan_Iassogna_Balls$plate_x > 0.833 | Dan_Iassogna_Balls$plate_x < -0.833)|(Dan_Iassogna_Balls$plate_z < Dan_Iassogna_Balls$sz_bot | Dan_Iassogna_Balls$plate_z > Dan_Iassogna_Balls$sz_top),'B','S')
table(Dan_Iassogna_Balls$AdjustedCall)

# Merge to create new dataset
Dan_Iassogna_AdjustedCalls = rbind(Dan_Iassogna_Strikes,Dan_Iassogna_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Dan_Iassogna)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Dan_Iassogna_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Dan_Iassogna_AdjustedCalls$AdjustedCall))







