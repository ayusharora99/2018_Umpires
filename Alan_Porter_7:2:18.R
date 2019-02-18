# 7/2/18 : Giants vs Rockies : Alan Porter

# 130 pitches were called strikes/balls

# The robot-ump called 36 of those pitches as called strikes & 94 as balls

# Alan Porter called 41 of those pitches as called strikes & 89 as balls 

# Accuracy: 93%

Alan_Porter <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Alan_Porter_7:2:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Alan_Porter)
names(Alan_Porter)
is.na(Alan_Porter)
colSums(is.na(Alan_Porter)) 
Alan_Porter = Alan_Porter[,colSums(is.na(Alan_Porter)) == 0] 
dim(Alan_Porter)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Alan_Porter = Alan_Porter[ , !(names(Alan_Porter) %in% drops)]
dim(Alan_Porter)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Alan_Porter_train = Alan_Porter[0:(0.8 * nrow(Alan_Porter)),]
dim(Alan_Porter_train)
prop.table(table(Alan_Porter_train$type))
Alan_Porter_test = Alan_Porter[(0.8*nrow(Alan_Porter)):nrow(Alan_Porter),]
dim(Alan_Porter_test)
prop.table(table(Alan_Porter_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Alan_Porter_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Alan_Porter_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Alan_Porter_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Alan_Porter_Strikes = subset(Alan_Porter, Alan_Porter$type == "S")
Alan_Porter_Balls = subset(Alan_Porter, Alan_Porter$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Alan_Porter_Strikes$AdjustedCall = ifelse((Alan_Porter_Strikes$plate_x < 0.833 & Alan_Porter_Strikes$plate_x > -0.833) & (Alan_Porter_Strikes$plate_z > Alan_Porter_Strikes$sz_bot & Alan_Porter_Strikes$plate_z < Alan_Porter_Strikes$sz_top), 'S', 'B')
table(Alan_Porter_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Alan_Porter_Balls$AdjustedCall = ifelse((Alan_Porter_Balls$plate_x > 0.833 | Alan_Porter_Balls$plate_x < -0.833)|(Alan_Porter_Balls$plate_z < Alan_Porter_Balls$sz_bot | Alan_Porter_Balls$plate_z > Alan_Porter_Balls$sz_top),'B','S')
table(Alan_Porter_Balls$AdjustedCall)

# Merge to create new dataset
Alan_Porter_AdjustedCalls = rbind(Alan_Porter_Strikes,Alan_Porter_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Alan_Porter)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Alan_Porter_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Alan_Porter_AdjustedCalls$AdjustedCall))







