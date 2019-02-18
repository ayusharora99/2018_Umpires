# 6/17/18 : Giants vs __OPPONENT__ : Adam Hamari

# 148 pitches were called strikes/balls

# The robot-ump called 42 of those pitches as called strikes & 106 as balls

# Adam Hamari called 46 of those pitches as called strikes & 102 as balls 

# Accuracy: 95%

Adam_Hamari <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Adam_Hamari_6:17:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Adam_Hamari)
names(Adam_Hamari)
is.na(Adam_Hamari)
colSums(is.na(Adam_Hamari)) 
Adam_Hamari = Adam_Hamari[,colSums(is.na(Adam_Hamari)) == 0] 
dim(Adam_Hamari)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Adam_Hamari = Adam_Hamari[ , !(names(Adam_Hamari) %in% drops)]
dim(Adam_Hamari)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Adam_Hamari_train = Adam_Hamari[0:(0.8 * nrow(Adam_Hamari)),]
dim(Adam_Hamari_train)
prop.table(table(Adam_Hamari_train$type))
Adam_Hamari_test = Adam_Hamari[(0.8*nrow(Adam_Hamari)):nrow(Adam_Hamari),]
dim(Adam_Hamari_test)
prop.table(table(Adam_Hamari_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Adam_Hamari_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Adam_Hamari_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Adam_Hamari_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Adam_Hamari_Strikes = subset(Adam_Hamari, Adam_Hamari$type == "S")
Adam_Hamari_Balls = subset(Adam_Hamari, Adam_Hamari$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Adam_Hamari_Strikes$AdjustedCall = ifelse((Adam_Hamari_Strikes$plate_x < 0.833 & Adam_Hamari_Strikes$plate_x > -0.833) & (Adam_Hamari_Strikes$plate_z > Adam_Hamari_Strikes$sz_bot & Adam_Hamari_Strikes$plate_z < Adam_Hamari_Strikes$sz_top), 'S', 'B')
table(Adam_Hamari_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Adam_Hamari_Balls$AdjustedCall = ifelse((Adam_Hamari_Balls$plate_x > 0.833 | Adam_Hamari_Balls$plate_x < -0.833)|(Adam_Hamari_Balls$plate_z < Adam_Hamari_Balls$sz_bot | Adam_Hamari_Balls$plate_z > Adam_Hamari_Balls$sz_top),'B','S')
table(Adam_Hamari_Balls$AdjustedCall)

# Merge to create new dataset
Adam_Hamari_AdjustedCalls = rbind(Adam_Hamari_Strikes,Adam_Hamari_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Adam_Hamari)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Adam_Hamari_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Adam_Hamari_AdjustedCalls$AdjustedCall))







