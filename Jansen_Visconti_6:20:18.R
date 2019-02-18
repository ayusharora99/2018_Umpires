# 6/20/18 : Giants vs __OPPONENT__ : Jansen Visconti

# 123 pitches were called strikes/balls

# The robot-ump called 42 of those pitches as called strikes & 81 as balls

# Jansen Visconti called 42 of those pitches as called strikes & 81 as balls 

# Accuracy: 89%

Jansen_Visconti <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jansen_Visconti_6:20:18.csv", header=TRUE)

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Jansen_Visconti)
names(Jansen_Visconti)
is.na(Jansen_Visconti)
colSums(is.na(Jansen_Visconti)) 
Jansen_Visconti = Jansen_Visconti[,colSums(is.na(Jansen_Visconti)) == 0] 
dim(Jansen_Visconti)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jansen_Visconti = Jansen_Visconti[ , !(names(Jansen_Visconti) %in% drops)]
dim(Jansen_Visconti)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Jansen_Visconti_train = Jansen_Visconti[0:(0.8 * nrow(Jansen_Visconti)),]
dim(Jansen_Visconti_train)
prop.table(table(Jansen_Visconti_train$type))
Jansen_Visconti_test = Jansen_Visconti[(0.8*nrow(Jansen_Visconti)):nrow(Jansen_Visconti),]
dim(Jansen_Visconti_test)
prop.table(table(Jansen_Visconti_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jansen_Visconti_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Jansen_Visconti_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Jansen_Visconti_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Jansen_Visconti_Strikes = subset(Jansen_Visconti, Jansen_Visconti$type == "S")
Jansen_Visconti_Balls = subset(Jansen_Visconti, Jansen_Visconti$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jansen_Visconti_Strikes$AdjustedCall = ifelse((Jansen_Visconti_Strikes$plate_x < 0.833 & Jansen_Visconti_Strikes$plate_x > -0.833) & (Jansen_Visconti_Strikes$plate_z > Jansen_Visconti_Strikes$sz_bot & Jansen_Visconti_Strikes$plate_z < Jansen_Visconti_Strikes$sz_top), 'S', 'B')
table(Jansen_Visconti_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jansen_Visconti_Balls$AdjustedCall = ifelse((Jansen_Visconti_Balls$plate_x > 0.833 | Jansen_Visconti_Balls$plate_x < -0.833)|(Jansen_Visconti_Balls$plate_z < Jansen_Visconti_Balls$sz_bot | Jansen_Visconti_Balls$plate_z > Jansen_Visconti_Balls$sz_top),'B','S')
table(Jansen_Visconti_Balls$AdjustedCall)

# Merge to create new dataset
Jansen_Visconti_AdjustedCalls = rbind(Jansen_Visconti_Strikes,Jansen_Visconti_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Jansen_Visconti)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Jansen_Visconti_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Jansen_Visconti_AdjustedCalls$AdjustedCall))







