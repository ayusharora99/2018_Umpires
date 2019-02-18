# 7/10/18 : Giants vs Cubs : Laz Diaz

# 137 pitches were called strikes/balls

# The robot-ump called 45 of those pitches as called strikes & 92 as balls

# Laz Diaz called 49 of those pitches as called strikes & 88 as balls 

# Accuracy: 90%

Laz_Diaz <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Laz_Diaz_7:10:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Laz_Diaz)
names(Laz_Diaz)
is.na(Laz_Diaz)
colSums(is.na(Laz_Diaz)) 
Laz_Diaz= Laz_Diaz[,colSums(is.na(Laz_Diaz)) == 0] 
dim(Laz_Diaz)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Laz_Diaz= Laz_Diaz[ , !(names(Laz_Diaz) %in% drops)]
dim(Laz_Diaz)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Laz_Diaz_train = Laz_Diaz[0:(0.8 * nrow(Laz_Diaz)),]
dim(Laz_Diaz_train)
prop.table(table(Laz_Diaz_train$type))
Laz_Diaz_test = Laz_Diaz[(0.8*nrow(Laz_Diaz)):nrow(Laz_Diaz),]
dim(Laz_Diaz_test)
prop.table(table(Laz_Diaz_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Laz_Diaz_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Laz_Diaz_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Laz_Diaz_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Laz_Diaz_Strikes = subset(Laz_Diaz, Laz_Diaz$type == "S")
Laz_Diaz_Balls = subset(Laz_Diaz, Laz_Diaz$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Laz_Diaz_Strikes$AdjustedCall = ifelse((Laz_Diaz_Strikes$plate_x < 0.833 & Laz_Diaz_Strikes$plate_x > -0.833) & (Laz_Diaz_Strikes$plate_z > Laz_Diaz_Strikes$sz_bot & Laz_Diaz_Strikes$plate_z < Laz_Diaz_Strikes$sz_top), 'S', 'B')
table(Laz_Diaz_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Laz_Diaz_Balls$AdjustedCall = ifelse((Laz_Diaz_Balls$plate_x > 0.833 | Laz_Diaz_Balls$plate_x < -0.833)|(Laz_Diaz_Balls$plate_z < Laz_Diaz_Balls$sz_bot | Laz_Diaz_Balls$plate_z > Laz_Diaz_Balls$sz_top),'B','S')
table(Laz_Diaz_Balls$AdjustedCall)

# Merge to create new dataset
Laz_Diaz_AdjustedCalls = rbind(Laz_Diaz_Strikes,Laz_Diaz_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Laz_Diaz)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Laz_Diaz_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Laz_Diaz_AdjustedCalls$AdjustedCall))







