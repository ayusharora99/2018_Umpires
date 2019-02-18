# 6/4/18 : Giants vs __OPPONENT__ : Lance Barrett

# 152 pitches were called strikes/balls

# The robot-ump called 40 of those pitches as called strikes & 112 as balls

# Lance Barrett called 49 of those pitches as called strikes & 103 as balls 

# Accuracy: 91%

Lance_Barrett <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Lance_Barrett_6:4:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Lance_Barrett)
names(Lance_Barrett)
is.na(Lance_Barrett)
colSums(is.na(Lance_Barrett)) 
Lance_Barrett = Lance_Barrett[,colSums(is.na(Lance_Barrett)) == 0] 
dim(Lance_Barrett)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Lance_Barrett = Lance_Barrett[ , !(names(Lance_Barrett) %in% drops)]
dim(Lance_Barrett)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Lance_Barrett_train = Lance_Barrett[0:(0.8 * nrow(Lance_Barrett)),]
dim(Lance_Barrett_train)
prop.table(table(Lance_Barrett_train$type))
Lance_Barrett_test = Lance_Barrett[(0.8*nrow(Lance_Barrett)):nrow(Lance_Barrett),]
dim(Lance_Barrett_test)
prop.table(table(Lance_Barrett_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Lance_Barrett_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Lance_Barrett_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Lance_Barrett_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Lance_Barrett_Strikes = subset(Lance_Barrett, Lance_Barrett$type == "S")
Lance_Barrett_Balls = subset(Lance_Barrett, Lance_Barrett$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Lance_Barrett_Strikes$AdjustedCall = ifelse((Lance_Barrett_Strikes$plate_x < 0.833 & Lance_Barrett_Strikes$plate_x > -0.833) & (Lance_Barrett_Strikes$plate_z > Lance_Barrett_Strikes$sz_bot & Lance_Barrett_Strikes$plate_z < Lance_Barrett_Strikes$sz_top), 'S', 'B')
table(Lance_Barrett_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Lance_Barrett_Balls$AdjustedCall = ifelse((Lance_Barrett_Balls$plate_x > 0.833 | Lance_Barrett_Balls$plate_x < -0.833)|(Lance_Barrett_Balls$plate_z < Lance_Barrett_Balls$sz_bot | Lance_Barrett_Balls$plate_z > Lance_Barrett_Balls$sz_top),'B','S')
table(Lance_Barrett_Balls$AdjustedCall)

# Merge to create new dataset
Lance_Barrett_AdjustedCalls = rbind(Lance_Barrett_Strikes,Lance_Barrett_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Lance_Barrett)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Lance_Barrett_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Lance_Barrett_AdjustedCalls$AdjustedCall))







