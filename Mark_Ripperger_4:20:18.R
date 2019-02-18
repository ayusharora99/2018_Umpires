# 4/20/18 : Giants vs __OPPONENT__ : Mark_Ripperger

# 140 pitches were called strikes/balls

# The robot-ump called 38 of those pitches as called strikes & 102 as balls

# Mark_Ripperger called 43 of those pitches as called strikes & 97 as balls 

# Accuracy: 89% 

Mark_Ripperger <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mark_Ripperger_4:20:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Mark_Ripperger)
names(Mark_Ripperger)
is.na(Mark_Ripperger)
colSums(is.na(Mark_Ripperger)) 
Mark_Ripperger = Mark_Ripperger[,colSums(is.na(Mark_Ripperger)) == 0] 
dim(Mark_Ripperger)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mark_Ripperger = Mark_Ripperger[ , !(names(Mark_Ripperger) %in% drops)]
dim(Mark_Ripperger)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Mark_Ripperger_train = Mark_Ripperger[0:(0.8 * nrow(Mark_Ripperger)),]
dim(Mark_Ripperger_train)
prop.table(table(Mark_Ripperger_train$type))
Mark_Ripperger_test = Mark_Ripperger[(0.8*nrow(Mark_Ripperger)):nrow(Mark_Ripperger),]
dim(Mark_Ripperger_test)
prop.table(table(Mark_Ripperger_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mark_Ripperger_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Mark_Ripperger_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Mark_Ripperger_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Mark_Ripperger_Strikes = subset(Mark_Ripperger, Mark_Ripperger$type == "S")
Mark_Ripperger_Balls = subset(Mark_Ripperger, Mark_Ripperger$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mark_Ripperger_Strikes$AdjustedCall = ifelse((Mark_Ripperger_Strikes$plate_x < 0.833 & Mark_Ripperger_Strikes$plate_x > -0.833) & (Mark_Ripperger_Strikes$plate_z > Mark_Ripperger_Strikes$sz_bot & Mark_Ripperger_Strikes$plate_z < Mark_Ripperger_Strikes$sz_top), 'S', 'B')
table(Mark_Ripperger_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mark_Ripperger_Balls$AdjustedCall = ifelse((Mark_Ripperger_Balls$plate_x > 0.833 | Mark_Ripperger_Balls$plate_x < -0.833)|(Mark_Ripperger_Balls$plate_z < Mark_Ripperger_Balls$sz_bot | Mark_Ripperger_Balls$plate_z > Mark_Ripperger_Balls$sz_top),'B','S')
table(Mark_Ripperger_Balls$AdjustedCall)

# Merge to create new dataset
Mark_Ripperger_AdjustedCalls = rbind(Mark_Ripperger_Strikes,Mark_Ripperger_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Mark_Ripperger)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Mark_Ripperger_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Mark_Ripperger_AdjustedCalls$AdjustedCall))







