#9/2/18: Giants vs Mets : Kerwin Danley

#101 pitches were called strikes/balls

#The robot-ump called 31 of those pitches as called strikes & 70 as balls

# Kerwin Danley called 34 of those pitches as called strikes & 67 as balls 

#Accuracy: 89%

Kerwin_Danley <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Kerwin_Danley_9:2:18.csv")
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Kerwin_Danley)
names(Kerwin_Danley)
is.na(Kerwin_Danley)
colSums(is.na(Kerwin_Danley)) 
Kerwin_Danley = Kerwin_Danley[,colSums(is.na(Kerwin_Danley)) == 0] 
dim(Kerwin_Danley)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Kerwin_Danley = Kerwin_Danley[ , !(names(Kerwin_Danley) %in% drops)]
# Kerwin_Danley = Kerwin_Danley[keep]
dim(Kerwin_Danley)
Kerwin_Danley

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Kerwin_Danley_train = Kerwin_Danley[0:(0.8 * nrow(Kerwin_Danley)),]
dim(Kerwin_Danley_train)
prop.table(table(Kerwin_Danley_train$type))
Kerwin_Danley_test = Kerwin_Danley[(0.8*nrow(Kerwin_Danley)):nrow(Kerwin_Danley),]
dim(Kerwin_Danley_test)
prop.table(table(Kerwin_Danley_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Kerwin_Danley_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_KD<-predict(tree_model, newdata = Kerwin_Danley_test, type = 'class')

confusionMatrix(table(prediction_KD, Kerwin_Danley_test$type))
confusionMatrix(prediction_KD,Kerwin_Danley_test$type)
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
Kerwin_Danley_Strikes = subset(Kerwin_Danley, Kerwin_Danley$type == "S")
Kerwin_Danley_Balls = subset(Kerwin_Danley, Kerwin_Danley$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Kerwin_Danley_Strikes$AdjustedCall = ifelse((Kerwin_Danley_Strikes$plate_x < 0.833 & Kerwin_Danley_Strikes$plate_x > -0.833) & (Kerwin_Danley_Strikes$plate_z > Kerwin_Danley_Strikes$sz_bot & Kerwin_Danley_Strikes$plate_z < Kerwin_Danley_Strikes$sz_top), 'S', 'B')
table(Kerwin_Danley_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Kerwin_Danley_Balls$AdjustedCall = ifelse((Kerwin_Danley_Balls$plate_x > 0.833 | Kerwin_Danley_Balls$plate_x < -0.833)|(Kerwin_Danley_Balls$plate_z < Kerwin_Danley_Balls$sz_bot | Kerwin_Danley_Balls$plate_z > Kerwin_Danley_Balls$sz_top),'B','S')
table(Kerwin_Danley_Balls$AdjustedCall)
# Merge to create new dataset
Kerwin_Danley_AdjustedCalls = rbind(Kerwin_Danley_Strikes,Kerwin_Danley_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Kerwin_Danley)
plot(tree_model)
text(tree_model, use.n = T)
prediction_KD<-predict(tree_model, newdata = Kerwin_Danley_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_KD,Kerwin_Danley_AdjustedCalls$AdjustedCall))

# Kerwin_Danley Strike Zone: 89.11% Accurate, 94% Precision, 90% Recall

