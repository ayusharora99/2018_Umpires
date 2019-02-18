# 4/13/18 : Giants vs __OPPONENT__ : Tim_Timmons

# 126 pitches were called strikes/balls

# The robot-ump called 41 of those pitches as called strikes & 85 as balls

# Tim_Timmons called 52 of those pitches as called strikes & 74 as balls 

# Accuracy: 87% 

Tim_Timmons <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Tim_Timmons_4:13:18.csv")
# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Tim_Timmons)
names(Tim_Timmons)
is.na(Tim_Timmons)
colSums(is.na(Tim_Timmons)) 
Tim_Timmons = Tim_Timmons[,colSums(is.na(Tim_Timmons)) == 0] 
dim(Tim_Timmons)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Tim_Timmons = Tim_Timmons[ , !(names(Tim_Timmons) %in% drops)]
dim(Tim_Timmons)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Tim_Timmons_train = Tim_Timmons[0:(0.8 * nrow(Tim_Timmons)),]
dim(Tim_Timmons_train)
prop.table(table(Tim_Timmons_train$type))
Tim_Timmons_test = Tim_Timmons[(0.8*nrow(Tim_Timmons)):nrow(Tim_Timmons),]
dim(Tim_Timmons_test)
prop.table(table(Tim_Timmons_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Tim_Timmons_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Tim_Timmons_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Tim_Timmons_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Tim_Timmons_Strikes = subset(Tim_Timmons, Tim_Timmons$type == "S")
Tim_Timmons_Balls = subset(Tim_Timmons, Tim_Timmons$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Tim_Timmons_Strikes$AdjustedCall = ifelse((Tim_Timmons_Strikes$plate_x < 0.833 & Tim_Timmons_Strikes$plate_x > -0.833) & (Tim_Timmons_Strikes$plate_z > Tim_Timmons_Strikes$sz_bot & Tim_Timmons_Strikes$plate_z < Tim_Timmons_Strikes$sz_top), 'S', 'B')
table(Tim_Timmons_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Tim_Timmons_Balls$AdjustedCall = ifelse((Tim_Timmons_Balls$plate_x > 0.833 | Tim_Timmons_Balls$plate_x < -0.833)|(Tim_Timmons_Balls$plate_z < Tim_Timmons_Balls$sz_bot | Tim_Timmons_Balls$plate_z > Tim_Timmons_Balls$sz_top),'B','S')
table(Tim_Timmons_Balls$AdjustedCall)

# Merge to create new dataset
Tim_Timmons_AdjustedCalls = rbind(Tim_Timmons_Strikes,Tim_Timmons_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Tim_Timmons)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Tim_Timmons_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Tim_Timmons_AdjustedCalls$AdjustedCall))







