#8/27/18: Giants vs Diamondbacks : DJ Reyburn

#159 pitches were called strikes/balls

#The robot-ump called 38 of those pitches as called strikes & 121 as balls

#DJ Reyburn called 53 of those pitches as called strikes & 106 as balls 

#Accuracy: 86%

DJ_Reyburn <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/DJ_Reyburn_7:26:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(DJ_Reyburn)
names(DJ_Reyburn)
is.na(DJ_Reyburn)
colSums(is.na(DJ_Reyburn)) 
DJ_Reyburn = DJ_Reyburn[,colSums(is.na(DJ_Reyburn)) == 0] 
dim(DJ_Reyburn)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
DJ_Reyburn = DJ_Reyburn[ , !(names(DJ_Reyburn) %in% drops)]
# DJ_Reyburn = DJ_Reyburn[keep]
dim(DJ_Reyburn)
DJ_Reyburn

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
DJ_Reyburn_train = DJ_Reyburn[0:(0.8 * nrow(DJ_Reyburn)),]
dim(DJ_Reyburn_train)
prop.table(table(DJ_Reyburn_train$type))
DJ_Reyburn_test = DJ_Reyburn[(0.8*nrow(DJ_Reyburn)):nrow(DJ_Reyburn),]
dim(DJ_Reyburn_test)
prop.table(table(DJ_Reyburn_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = DJ_Reyburn_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = DJ_Reyburn_test, type = 'class')

confusionMatrix(table(prediction_BG, DJ_Reyburn_test$type))
confusionMatrix(prediction_BG,DJ_Reyburn_test$type)
# 78.57% Accuracy, 82.35% Precision, 82.35% Recall w/ Keeps
# 100% in ALL w/ Drops
#sensitivity, specificity which is more important: 1.0000, 1.0000


#  Accuracy, precision, recall,sensitivity, specificity, ROC, AUC, Kappa, F score, LogLoss etc
# Run ROC for imbalanced data

# Kmeans Clustering
# https://www.youtube.com/watch?v=4R8nWDh-wA0&index=5&list=PLaozD487rOR_LgH8ObhA2-ns2ijiB-uaf 
# Hierarichal Clustering?

# Check for overfitting & underfitting

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls

# Seperate Ball & Strike Types
DJ_Reyburn_Strikes = subset(DJ_Reyburn, DJ_Reyburn$type == "S")
DJ_Reyburn_Balls = subset(DJ_Reyburn, DJ_Reyburn$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
DJ_Reyburn_Strikes$AdjustedCall = ifelse((DJ_Reyburn_Strikes$plate_x < 0.833 & DJ_Reyburn_Strikes$plate_x > -0.833) & (DJ_Reyburn_Strikes$plate_z > DJ_Reyburn_Strikes$sz_bot & DJ_Reyburn_Strikes$plate_z < DJ_Reyburn_Strikes$sz_top), 'S', 'B')
table(DJ_Reyburn_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
DJ_Reyburn_Balls$AdjustedCall = ifelse((DJ_Reyburn_Balls$plate_x > 0.833 | DJ_Reyburn_Balls$plate_x < -0.833)|(DJ_Reyburn_Balls$plate_z < DJ_Reyburn_Balls$sz_bot | DJ_Reyburn_Balls$plate_z > DJ_Reyburn_Balls$sz_top),'B','S')
table(DJ_Reyburn_Balls$AdjustedCall)
# Merge to create new dataset
DJ_Reyburn_AdjustedCalls = rbind(DJ_Reyburn_Strikes,DJ_Reyburn_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = DJ_Reyburn)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = DJ_Reyburn_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,DJ_Reyburn_AdjustedCalls$AdjustedCall))



