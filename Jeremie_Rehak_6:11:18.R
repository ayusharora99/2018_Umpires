# 6/11/18 : Giants vs __OPPONENT__ : Jeremie Rehak

# 148 pitches were called strikes/balls

# The robot-ump called 38 of those pitches as called strikes & 110 as balls

# Jeremie Rehak called 37 of those pitches as called strikes & 111 as balls 

# Accuracy: 95%

Jeremie_Rehak <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jeremie_Rehak_6:11:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Jeremie_Rehak)
names(Jeremie_Rehak)
is.na(Jeremie_Rehak)
colSums(is.na(Jeremie_Rehak)) 
Jeremie_Rehak = Jeremie_Rehak[,colSums(is.na(Jeremie_Rehak)) == 0] 
dim(Jeremie_Rehak)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jeremie_Rehak = Jeremie_Rehak[ , !(names(Jeremie_Rehak) %in% drops)]
dim(Jeremie_Rehak)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Jeremie_Rehak_train = Jeremie_Rehak[0:(0.8 * nrow(Jeremie_Rehak)),]
dim(Jeremie_Rehak_train)
prop.table(table(Jeremie_Rehak_train$type))
Jeremie_Rehak_test = Jeremie_Rehak[(0.8*nrow(Jeremie_Rehak)):nrow(Jeremie_Rehak),]
dim(Jeremie_Rehak_test)
prop.table(table(Jeremie_Rehak_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jeremie_Rehak_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Jeremie_Rehak_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Jeremie_Rehak_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Jeremie_Rehak_Strikes = subset(Jeremie_Rehak, Jeremie_Rehak$type == "S")
Jeremie_Rehak_Balls = subset(Jeremie_Rehak, Jeremie_Rehak$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jeremie_Rehak_Strikes$AdjustedCall = ifelse((Jeremie_Rehak_Strikes$plate_x < 0.833 & Jeremie_Rehak_Strikes$plate_x > -0.833) & (Jeremie_Rehak_Strikes$plate_z > Jeremie_Rehak_Strikes$sz_bot & Jeremie_Rehak_Strikes$plate_z < Jeremie_Rehak_Strikes$sz_top), 'S', 'B')
table(Jeremie_Rehak_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jeremie_Rehak_Balls$AdjustedCall = ifelse((Jeremie_Rehak_Balls$plate_x > 0.833 | Jeremie_Rehak_Balls$plate_x < -0.833)|(Jeremie_Rehak_Balls$plate_z < Jeremie_Rehak_Balls$sz_bot | Jeremie_Rehak_Balls$plate_z > Jeremie_Rehak_Balls$sz_top),'B','S')
table(Jeremie_Rehak_Balls$AdjustedCall)

# Merge to create new dataset
Jeremie_Rehak_AdjustedCalls = rbind(Jeremie_Rehak_Strikes,Jeremie_Rehak_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Jeremie_Rehak)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Jeremie_Rehak_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Jeremie_Rehak_AdjustedCalls$AdjustedCall))







