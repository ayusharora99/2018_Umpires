# 4/3/18 : Giants vs __OPPONENT__ : Ted Barrett

# 140 pitches were called strikes/balls

# The robot-ump called 37 of those pitches as called strikes & 103 as balls

# __UMPIRE__ called 50 of those pitches as called strikes & 90 as balls 

# Accuracy: 85% 

Ted_Barrett <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Ted_Barrett_4:3:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Ted_Barrett)
names(Ted_Barrett)
is.na(Ted_Barrett)
colSums(is.na(Ted_Barrett)) 
Ted_Barrett = Ted_Barrett[,colSums(is.na(Ted_Barrett)) == 0] 
dim(Ted_Barrett)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Ted_Barrett = Ted_Barrett[ , !(names(Ted_Barrett) %in% drops)]
dim(Ted_Barrett)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Ted_Barrett_train = Ted_Barrett[0:(0.8 * nrow(Ted_Barrett)),]
dim(Ted_Barrett_train)
prop.table(table(Ted_Barrett_train$type))
Ted_Barrett_test = Ted_Barrett[(0.8*nrow(Ted_Barrett)):nrow(Ted_Barrett),]
dim(Ted_Barrett_test)
prop.table(table(Ted_Barrett_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Ted_Barrett_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Ted_Barrett_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Ted_Barrett_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Ted_Barrett_Strikes = subset(Ted_Barrett, Ted_Barrett$type == "S")
Ted_Barrett_Balls = subset(Ted_Barrett, Ted_Barrett$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Ted_Barrett_Strikes$AdjustedCall = ifelse((Ted_Barrett_Strikes$plate_x < 0.833 & Ted_Barrett_Strikes$plate_x > -0.833) & (Ted_Barrett_Strikes$plate_z > Ted_Barrett_Strikes$sz_bot & Ted_Barrett_Strikes$plate_z < Ted_Barrett_Strikes$sz_top), 'S', 'B')
table(Ted_Barrett_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Ted_Barrett_Balls$AdjustedCall = ifelse((Ted_Barrett_Balls$plate_x > 0.833 | Ted_Barrett_Balls$plate_x < -0.833)|(Ted_Barrett_Balls$plate_z < Ted_Barrett_Balls$sz_bot | Ted_Barrett_Balls$plate_z > Ted_Barrett_Balls$sz_top),'B','S')
table(Ted_Barrett_Balls$AdjustedCall)

# Merge to create new dataset
Ted_Barrett_AdjustedCalls = rbind(Ted_Barrett_Strikes,Ted_Barrett_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Ted_Barrett)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Ted_Barrett_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Ted_Barrett_AdjustedCalls$AdjustedCall))







