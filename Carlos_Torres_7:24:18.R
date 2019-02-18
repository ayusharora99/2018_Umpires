#7/24/18: Giants vs Mariners : Carlos Torres

#138 pitches were called strikes/balls

#The robot-ump called 42 of those pitches as called strikes & 96 as balls

#Carlos Torres called 47 of those pitches as called strikes & 91 as balls 

#Accuracy: 89%

Carlos_Torres <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Carlos_Torres_7:24:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Carlos_Torres)
names(Carlos_Torres)
is.na(Carlos_Torres)
colSums(is.na(Carlos_Torres)) 
Carlos_Torres = Carlos_Torres[,colSums(is.na(Carlos_Torres)) == 0] 
dim(Carlos_Torres)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Carlos_Torres = Carlos_Torres[ , !(names(Carlos_Torres) %in% drops)]
# Carlos_Torres = Carlos_Torres[keep]
dim(Carlos_Torres)
Carlos_Torres

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Carlos_Torres_train = Carlos_Torres[0:(0.8 * nrow(Carlos_Torres)),]
dim(Carlos_Torres_train)
prop.table(table(Carlos_Torres_train$type))
Carlos_Torres_test = Carlos_Torres[(0.8*nrow(Carlos_Torres)):nrow(Carlos_Torres),]
dim(Carlos_Torres_test)
prop.table(table(Carlos_Torres_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Carlos_Torres_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Carlos_Torres_test, type = 'class')

confusionMatrix(table(prediction_BG, Carlos_Torres_test$type))
confusionMatrix(prediction_BG,Carlos_Torres_test$type)
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
Carlos_Torres_Strikes = subset(Carlos_Torres, Carlos_Torres$type == "S")
Carlos_Torres_Balls = subset(Carlos_Torres, Carlos_Torres$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Carlos_Torres_Strikes$AdjustedCall = ifelse((Carlos_Torres_Strikes$plate_x < 0.833 & Carlos_Torres_Strikes$plate_x > -0.833) & (Carlos_Torres_Strikes$plate_z > Carlos_Torres_Strikes$sz_bot & Carlos_Torres_Strikes$plate_z < Carlos_Torres_Strikes$sz_top), 'S', 'B')
table(Carlos_Torres_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Carlos_Torres_Balls$AdjustedCall = ifelse((Carlos_Torres_Balls$plate_x > 0.833 | Carlos_Torres_Balls$plate_x < -0.833)|(Carlos_Torres_Balls$plate_z < Carlos_Torres_Balls$sz_bot | Carlos_Torres_Balls$plate_z > Carlos_Torres_Balls$sz_top),'B','S')
table(Carlos_Torres_Balls$AdjustedCall)
# Merge to create new dataset
Carlos_Torres_AdjustedCalls = rbind(Carlos_Torres_Strikes,Carlos_Torres_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Carlos_Torres)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Carlos_Torres_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Carlos_Torres_AdjustedCalls$AdjustedCall))



