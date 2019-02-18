#8/27/18: Giants vs Reds : Phil Cuzzi

#141 pitches were called strikes/balls

#The robot-ump called 43 of those pitches as called strikes & 98 as balls

#Phil Cuzzi called 47 of those pitches as called strikes & 94 as balls 

#Accuracy: 91%

Phil_Cuzzi <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Phil_Cuzzi_8:19:18.csv")
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Phil_Cuzzi)
names(Phil_Cuzzi)
is.na(Phil_Cuzzi)
colSums(is.na(Phil_Cuzzi)) 
Phil_Cuzzi = Phil_Cuzzi[,colSums(is.na(Phil_Cuzzi)) == 0] 
dim(Phil_Cuzzi)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Phil_Cuzzi = Phil_Cuzzi[ , !(names(Phil_Cuzzi) %in% drops)]
# Phil_Cuzzi = Phil_Cuzzi[keep]
dim(Phil_Cuzzi)
Phil_Cuzzi

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Phil_Cuzzi_train = Phil_Cuzzi[0:(0.8 * nrow(Phil_Cuzzi)),]
dim(Phil_Cuzzi_train)
prop.table(table(Phil_Cuzzi_train$type))
Phil_Cuzzi_test = Phil_Cuzzi[(0.8*nrow(Phil_Cuzzi)):nrow(Phil_Cuzzi),]
dim(Phil_Cuzzi_test)
prop.table(table(Phil_Cuzzi_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Phil_Cuzzi_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_PC<-predict(tree_model, newdata = Phil_Cuzzi_test, type = 'class')

confusionMatrix(table(prediction_PC, Phil_Cuzzi_test$type))
confusionMatrix(prediction_PC,Phil_Cuzzi_test$type)
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
Phil_Cuzzi_Strikes = subset(Phil_Cuzzi, Phil_Cuzzi$type == "S")
Phil_Cuzzi_Balls = subset(Phil_Cuzzi, Phil_Cuzzi$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Phil_Cuzzi_Strikes$AdjustedCall = ifelse((Phil_Cuzzi_Strikes$plate_x < 0.833 & Phil_Cuzzi_Strikes$plate_x > -0.833) & (Phil_Cuzzi_Strikes$plate_z > Phil_Cuzzi_Strikes$sz_bot & Phil_Cuzzi_Strikes$plate_z < Phil_Cuzzi_Strikes$sz_top), 'S', 'B')
table(Phil_Cuzzi_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Phil_Cuzzi_Balls$AdjustedCall = ifelse((Phil_Cuzzi_Balls$plate_x > 0.833 | Phil_Cuzzi_Balls$plate_x < -0.833)|(Phil_Cuzzi_Balls$plate_z < Phil_Cuzzi_Balls$sz_bot | Phil_Cuzzi_Balls$plate_z > Phil_Cuzzi_Balls$sz_top),'B','S')
table(Phil_Cuzzi_Balls$AdjustedCall)
# Merge to create new dataset
Phil_Cuzzi_AdjustedCalls = rbind(Phil_Cuzzi_Strikes,Phil_Cuzzi_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Phil_Cuzzi)
plot(tree_model)
text(tree_model, use.n = T)
prediction_PC<-predict(tree_model, newdata = Phil_Cuzzi_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_PC,Phil_Cuzzi_AdjustedCalls$AdjustedCall))



