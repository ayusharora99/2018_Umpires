# 6/22/18 : Giants vs __OPPONENT__ : Brian Knight

# 132 pitches were called strikes/balls

# The robot-ump called 31 of those pitches as called strikes & 101 as balls

# Brian Knight called 39 of those pitches as called strikes & 93 as balls 

# Accuracy: 89%

Brian_Knight <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Brian_Knight_6:22:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Brian_Knight)
names(Brian_Knight)
is.na(Brian_Knight)
colSums(is.na(Brian_Knight)) 
Brian_Knight = Brian_Knight[,colSums(is.na(Brian_Knight)) == 0] 
dim(Brian_Knight)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Brian_Knight = Brian_Knight[ , !(names(Brian_Knight) %in% drops)]
dim(Brian_Knight)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Brian_Knight_train = Brian_Knight[0:(0.8 * nrow(Brian_Knight)),]
dim(Brian_Knight_train)
prop.table(table(Brian_Knight_train$type))
Brian_Knight_test = Brian_Knight[(0.8*nrow(Brian_Knight)):nrow(Brian_Knight),]
dim(Brian_Knight_test)
prop.table(table(Brian_Knight_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Brian_Knight_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Brian_Knight_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Brian_Knight_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Brian_Knight_Strikes = subset(Brian_Knight, Brian_Knight$type == "S")
Brian_Knight_Balls = subset(Brian_Knight, Brian_Knight$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Brian_Knight_Strikes$AdjustedCall = ifelse((Brian_Knight_Strikes$plate_x < 0.833 & Brian_Knight_Strikes$plate_x > -0.833) & (Brian_Knight_Strikes$plate_z > Brian_Knight_Strikes$sz_bot & Brian_Knight_Strikes$plate_z < Brian_Knight_Strikes$sz_top), 'S', 'B')
table(Brian_Knight_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Brian_Knight_Balls$AdjustedCall = ifelse((Brian_Knight_Balls$plate_x > 0.833 | Brian_Knight_Balls$plate_x < -0.833)|(Brian_Knight_Balls$plate_z < Brian_Knight_Balls$sz_bot | Brian_Knight_Balls$plate_z > Brian_Knight_Balls$sz_top),'B','S')
table(Brian_Knight_Balls$AdjustedCall)

# Merge to create new dataset
Brian_Knight_AdjustedCalls = rbind(Brian_Knight_Strikes,Brian_Knight_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Brian_Knight)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Brian_Knight_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Brian_Knight_AdjustedCalls$AdjustedCall))







