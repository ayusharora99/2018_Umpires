#8/27/18: Giants vs Diamondbacks : Brian Gorman

#145 pitches were called strikes/balls

#The robot-ump called 48 of those pitches as called strikes & 97 as balls

#Brian Gorman called 55 of those pitches as called strikes & 90 as balls 

#Accuracy: 88%

Brian_Gorman <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Brian_Gorman_8:27.18.csv", header=TRUE)
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Brian_Gorman)
names(Brian_Gorman)
is.na(Brian_Gorman)
colSums(is.na(Brian_Gorman)) 
Brian_Gorman = Brian_Gorman[,colSums(is.na(Brian_Gorman)) == 0] 
dim(Brian_Gorman)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Brian_Gorman = Brian_Gorman[ , !(names(Brian_Gorman) %in% drops)]
# Brian_Gorman = Brian_Gorman[keep]
dim(Brian_Gorman)
Brian_Gorman

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Brian_Gorman_train = Brian_Gorman[0:(0.8 * nrow(Brian_Gorman)),]
dim(Brian_Gorman_train)
prop.table(table(Brian_Gorman_train$type))
Brian_Gorman_test = Brian_Gorman[(0.8*nrow(Brian_Gorman)):nrow(Brian_Gorman),]
dim(Brian_Gorman_test)
prop.table(table(Brian_Gorman_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Brian_Gorman_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
prediction_BG<-predict(tree_model, newdata = Brian_Gorman_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(prediction_BG, Brian_Gorman_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Brian_Gorman_Strikes = subset(Brian_Gorman, Brian_Gorman$type == "S")
Brian_Gorman_Balls = subset(Brian_Gorman, Brian_Gorman$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Brian_Gorman_Strikes$AdjustedCall = ifelse((Brian_Gorman_Strikes$plate_x < 0.833 & Brian_Gorman_Strikes$plate_x > -0.833) & (Brian_Gorman_Strikes$plate_z > Brian_Gorman_Strikes$sz_bot & Brian_Gorman_Strikes$plate_z < Brian_Gorman_Strikes$sz_top), 'S', 'B')
table(Brian_Gorman_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Brian_Gorman_Balls$AdjustedCall = ifelse((Brian_Gorman_Balls$plate_x > 0.833 | Brian_Gorman_Balls$plate_x < -0.833)|(Brian_Gorman_Balls$plate_z < Brian_Gorman_Balls$sz_bot | Brian_Gorman_Balls$plate_z > Brian_Gorman_Balls$sz_top),'B','S')
table(Brian_Gorman_Balls$AdjustedCall)

# Merge to create new dataset
Brian_Gorman_AdjustedCalls = rbind(Brian_Gorman_Strikes,Brian_Gorman_Balls)

# Predict using Umpire's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Brian_Gorman)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Brian_Gorman_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Brian_Gorman_AdjustedCalls$AdjustedCall))



