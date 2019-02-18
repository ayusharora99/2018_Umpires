#8/23/18: Giants vs Diamondbacks : Tonny Randazzo

#133 pitches were called strikes/balls

#The robot-ump called 42 of those pitches as called strikes & 91 as balls

# Tonny Randazzo called 44 of those pitches as called strikes & 90 as balls 

#Accuracy: 87%

Tony_Randazzo <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Tony_Randazzo_8:23:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Tony_Randazzo)
names(Tony_Randazzo)
is.na(Tony_Randazzo)
colSums(is.na(Tony_Randazzo)) 
Tony_Randazzo = Tony_Randazzo[,colSums(is.na(Tony_Randazzo)) == 0] 
dim(Tony_Randazzo)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Tony_Randazzo = Tony_Randazzo[ , !(names(Tony_Randazzo) %in% drops)]
# Tony_Randazzo = Tony_Randazzo[keep]
dim(Tony_Randazzo)
Tony_Randazzo

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Tony_Randazzo_train = Tony_Randazzo[0:(0.8 * nrow(Tony_Randazzo)),]
dim(Tony_Randazzo_train)
prop.table(table(Tony_Randazzo_train$type))
Tony_Randazzo_test = Tony_Randazzo[(0.8*nrow(Tony_Randazzo)):nrow(Tony_Randazzo),]
dim(Tony_Randazzo_test)
prop.table(table(Tony_Randazzo_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Tony_Randazzo_train)
plot(tree_model)
text(tree_model, use.n = T)
prediciton_TR<-predict(tree_model, newdata = Tony_Randazzo_test, type = 'class')

confusionMatrix(table(prediciton_TR, Tony_Randazzo_test$type))
confusionMatrix(prediciton_TR,Tony_Randazzo_test$type)
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
Tony_Randazzo_Strikes = subset(Tony_Randazzo, Tony_Randazzo$type == "S")
Tony_Randazzo_Balls = subset(Tony_Randazzo, Tony_Randazzo$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Tony_Randazzo_Strikes$AdjustedCall = ifelse((Tony_Randazzo_Strikes$plate_x < 0.833 & Tony_Randazzo_Strikes$plate_x > -0.833) & (Tony_Randazzo_Strikes$plate_z > Tony_Randazzo_Strikes$sz_bot & Tony_Randazzo_Strikes$plate_z < Tony_Randazzo_Strikes$sz_top), 'S', 'B')
table(Tony_Randazzo_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Tony_Randazzo_Balls$AdjustedCall = ifelse((Tony_Randazzo_Balls$plate_x > 0.833 | Tony_Randazzo_Balls$plate_x < -0.833)|(Tony_Randazzo_Balls$plate_z < Tony_Randazzo_Balls$sz_bot | Tony_Randazzo_Balls$plate_z > Tony_Randazzo_Balls$sz_top),'B','S')
table(Tony_Randazzo_Balls$AdjustedCall)
# Merge to create new dataset
Tony_Randazzo_AdjustedCalls = rbind(Tony_Randazzo_Strikes,Tony_Randazzo_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Tony_Randazzo)
plot(tree_model)
text(tree_model, use.n = T)
prediciton_TR<-predict(tree_model, newdata = Tony_Randazzo_AdjustedCalls, type = 'class')
confusionMatrix(table(prediciton_TR,Tony_Randazzo_AdjustedCalls$AdjustedCall))



