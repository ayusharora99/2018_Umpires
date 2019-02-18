chadfairchild <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chad_Fairchild_9:1:18.csv")

library(e1071)
library(caret)
library(rpart)
install.packages(c("e1071","caret","rpart"))
dim(chadfairchild)
names(chadfairchild)
is.na(chadfairchild)
colSums(is.na(chadfairchild)) 
chadfairchild = chadfairchild[,colSums(is.na(chadfairchild)) == 0] 
dim(chadfairchild)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
chadfairchild = chadfairchild[ , !(names(chadfairchild) %in% drops)]
chadfairchild = chadfairchild[keep]
dim(chadfairchild)
chadfairchild

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
chadfairchild_test = chadfairchild[139:173,]
dim(chadfairchild_test)
prop.table(table(chadfairchild_test$type))
chadfairchild_train = chadfairchild[0:138,]
dim(chadfairchild_train)
prop.table(table(chadfairchild_train$type))

# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = chadfairchild_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CF<-predict(tree_model, newdata = chadfairchild_test, type = 'class')

confusionMatrix(table(prediction_CF, chadfairchild_test$type))
confusionMatrix(prediction_CF,chadfairchild_test$type)
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
chadfairchild_Strikes = subset(chadfairchild, chadfairchild$type == "S")
chadfairchild_Balls = subset(chadfairchild, chadfairchild$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
chadfairchild_Strikes$AdjustedCall = ifelse((chadfairchild_Strikes$plate_x < 0.833 & chadfairchild_Strikes$plate_x > -0.833) & (chadfairchild_Strikes$plate_z > chadfairchild_Strikes$sz_bot & chadfairchild_Strikes$plate_z < chadfairchild_Strikes$sz_top), 'S', 'B')
table(chadfairchild_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
chadfairchild_Balls$AdjustedCall = ifelse((chadfairchild_Balls$plate_x > 0.833 | chadfairchild_Balls$plate_x < -0.833)|(chadfairchild_Balls$plate_z < chadfairchild_Balls$sz_bot | chadfairchild_Balls$plate_z > chadfairchild_Balls$sz_top),'B','S')
table(chadfairchild_Balls$AdjustedCall)
# Merge to create new dataset
chadfairchild_AdjustedCalls = rbind(chadfairchild_Strikes,chadfairchild_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = chadfairchild)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CF<-predict(tree_model, newdata = chadfairchild_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_CF,chadfairchild_AdjustedCalls$AdjustedCall))

# Chad Fairchild Strike Zone: 92.49% Accurate, 100% Precision, 89% Recall

