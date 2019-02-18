# 4/21/18 : Giants vs __OPPONENT__ : Joe_West

# 158 pitches were called strikes/balls

# The robot-ump called 48 of those pitches as called strikes & 110 as balls

# Joe_West called 60 of those pitches as called strikes & 98 as balls 

# Accuracy: 89% 

Joe_West <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Joe_West_4:21:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Joe_West)
names(Joe_West)
is.na(Joe_West)
colSums(is.na(Joe_West)) 
Joe_West = Joe_West[,colSums(is.na(Joe_West)) == 0] 
dim(Joe_West)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Joe_West = Joe_West[ , !(names(Joe_West) %in% drops)]
dim(Joe_West)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Joe_West_train = Joe_West[0:(0.8 * nrow(Joe_West)),]
dim(Joe_West_train)
prop.table(table(Joe_West_train$type))
Joe_West_test = Joe_West[(0.8*nrow(Joe_West)):nrow(Joe_West),]
dim(Joe_West_test)
prop.table(table(Joe_West_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Joe_West_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Joe_West_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Joe_West_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Joe_West_Strikes = subset(Joe_West, Joe_West$type == "S")
Joe_West_Balls = subset(Joe_West, Joe_West$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Joe_West_Strikes$AdjustedCall = ifelse((Joe_West_Strikes$plate_x < 0.833 & Joe_West_Strikes$plate_x > -0.833) & (Joe_West_Strikes$plate_z > Joe_West_Strikes$sz_bot & Joe_West_Strikes$plate_z < Joe_West_Strikes$sz_top), 'S', 'B')
table(Joe_West_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Joe_West_Balls$AdjustedCall = ifelse((Joe_West_Balls$plate_x > 0.833 | Joe_West_Balls$plate_x < -0.833)|(Joe_West_Balls$plate_z < Joe_West_Balls$sz_bot | Joe_West_Balls$plate_z > Joe_West_Balls$sz_top),'B','S')
table(Joe_West_Balls$AdjustedCall)

# Merge to create new dataset
Joe_West_AdjustedCalls = rbind(Joe_West_Strikes,Joe_West_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Joe_West)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Joe_West_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Joe_West_AdjustedCalls$AdjustedCall))







