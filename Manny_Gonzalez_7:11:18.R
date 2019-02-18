# 7/11/18 : Giants vs Cubs : Manny Gonzalez

# 233 pitches were called strikes/balls

# The robot-ump called 71 of those pitches as called strikes & 162 as balls

# Manny Gonzalez called 69 of those pitches as called strikes & 164 as balls 

# Accuracy: 91%

Manny_Gonzalez <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Manny_Gonzalez_7:11:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Manny_Gonzalez)
names(Manny_Gonzalez)
is.na(Manny_Gonzalez)
colSums(is.na(Manny_Gonzalez)) 
Manny_Gonzalez = Manny_Gonzalez[,colSums(is.na(Manny_Gonzalez)) == 0] 
dim(Manny_Gonzalez)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Manny_Gonzalez = Manny_Gonzalez[ , !(names(Manny_Gonzalez) %in% drops)]
dim(Manny_Gonzalez)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Manny_Gonzalez_train = Manny_Gonzalez[0:(0.8 * nrow(Manny_Gonzalez)),]
dim(Manny_Gonzalez_train)
prop.table(table(Manny_Gonzalez_train$type))
Manny_Gonzalez_test = Manny_Gonzalez[(0.8*nrow(Manny_Gonzalez)):nrow(Manny_Gonzalez),]
dim(Manny_Gonzalez_test)
prop.table(table(Manny_Gonzalez_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Manny_Gonzalez_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Manny_Gonzalez_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Manny_Gonzalez_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Manny_Gonzalez_Strikes = subset(Manny_Gonzalez, Manny_Gonzalez$type == "S")
Manny_Gonzalez_Balls = subset(Manny_Gonzalez, Manny_Gonzalez$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Manny_Gonzalez_Strikes$AdjustedCall = ifelse((Manny_Gonzalez_Strikes$plate_x < 0.833 & Manny_Gonzalez_Strikes$plate_x > -0.833) & (Manny_Gonzalez_Strikes$plate_z > Manny_Gonzalez_Strikes$sz_bot & Manny_Gonzalez_Strikes$plate_z < Manny_Gonzalez_Strikes$sz_top), 'S', 'B')
table(Manny_Gonzalez_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Manny_Gonzalez_Balls$AdjustedCall = ifelse((Manny_Gonzalez_Balls$plate_x > 0.833 | Manny_Gonzalez_Balls$plate_x < -0.833)|(Manny_Gonzalez_Balls$plate_z < Manny_Gonzalez_Balls$sz_bot | Manny_Gonzalez_Balls$plate_z > Manny_Gonzalez_Balls$sz_top),'B','S')
table(Manny_Gonzalez_Balls$AdjustedCall)

# Merge to create new dataset
Manny_Gonzalez_AdjustedCalls = rbind(Manny_Gonzalez_Strikes,Manny_Gonzalez_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Manny_Gonzalez)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Manny_Gonzalez_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Manny_Gonzalez_AdjustedCalls$AdjustedCall))







