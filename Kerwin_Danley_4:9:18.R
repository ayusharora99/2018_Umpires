# 4/9/18 : Giants vs __OPPONENT__ : Kerwin Danley

# 139 pitches were called strikes/balls

# The robot-ump called 38 of those pitches as called strikes & 104 as balls

# Kerwin Danley called 50 of those pitches as called strikes & 89 as balls 

# Accuracy: 87% 

Kerwin_Danley <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Kerwin_Danley_4:9:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Kerwin_Danley)
names(Kerwin_Danley)
is.na(Kerwin_Danley)
colSums(is.na(Kerwin_Danley)) 
Kerwin_Danley = Kerwin_Danley[,colSums(is.na(Kerwin_Danley)) == 0] 
dim(Kerwin_Danley)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Kerwin_Danley = Kerwin_Danley[ , !(names(Kerwin_Danley) %in% drops)]
dim(Kerwin_Danley)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Kerwin_Danley_train = Kerwin_Danley[0:(0.8 * nrow(Kerwin_Danley)),]
dim(Kerwin_Danley_train)
prop.table(table(Kerwin_Danley_train$type))
Kerwin_Danley_test = Kerwin_Danley[(0.8*nrow(Kerwin_Danley)):nrow(Kerwin_Danley),]
dim(Kerwin_Danley_test)
prop.table(table(Kerwin_Danley_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Kerwin_Danley_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Kerwin_Danley_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Kerwin_Danley_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Kerwin_Danley_Strikes = subset(Kerwin_Danley, Kerwin_Danley$type == "S")
Kerwin_Danley_Balls = subset(Kerwin_Danley, Kerwin_Danley$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Kerwin_Danley_Strikes$AdjustedCall = ifelse((Kerwin_Danley_Strikes$plate_x < 0.833 & Kerwin_Danley_Strikes$plate_x > -0.833) & (Kerwin_Danley_Strikes$plate_z > Kerwin_Danley_Strikes$sz_bot & Kerwin_Danley_Strikes$plate_z < Kerwin_Danley_Strikes$sz_top), 'S', 'B')
table(Kerwin_Danley_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Kerwin_Danley_Balls$AdjustedCall = ifelse((Kerwin_Danley_Balls$plate_x > 0.833 | Kerwin_Danley_Balls$plate_x < -0.833)|(Kerwin_Danley_Balls$plate_z < Kerwin_Danley_Balls$sz_bot | Kerwin_Danley_Balls$plate_z > Kerwin_Danley_Balls$sz_top),'B','S')
table(Kerwin_Danley_Balls$AdjustedCall)

# Merge to create new dataset
Kerwin_Danley_AdjustedCalls = rbind(Kerwin_Danley_Strikes,Kerwin_Danley_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Kerwin_Danley)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Kerwin_Danley_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Kerwin_Danley_AdjustedCalls$AdjustedCall))







