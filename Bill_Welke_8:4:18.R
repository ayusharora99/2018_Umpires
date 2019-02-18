#8/4/18: Giants vs Diamondbacks : Bill Welke

#144 pitches were called strikes/balls

#The robot-ump called 99 of those pitches as called strikes & 45 as balls

#Bill Welke called 93 of those pitches as called strikes & 51 as balls 

#Accuracy: 92%

Bill_Welke <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Bill_Welke_8:4:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Bill_Welke)
names(Bill_Welke)
is.na(Bill_Welke)
colSums(is.na(Bill_Welke)) 
Bill_Welke = Bill_Welke[,colSums(is.na(Bill_Welke)) == 0] 
dim(Bill_Welke)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Bill_Welke = Bill_Welke[ , !(names(Bill_Welke) %in% drops)]
# Bill_Welke = Bill_Welke[keep]
dim(Bill_Welke)
Bill_Welke

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Bill_Welke_train = Bill_Welke[0:(0.8 * nrow(Bill_Welke)),]
dim(Bill_Welke_train)
prop.table(table(Bill_Welke_train$type))
Bill_Welke_test = Bill_Welke[(0.8*nrow(Bill_Welke)):nrow(Bill_Welke),]
dim(Bill_Welke_test)
prop.table(table(Bill_Welke_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Bill_Welke_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Bill_Welke_test, type = 'class')

confusionMatrix(table(prediction_BG, Bill_Welke_test$type))
confusionMatrix(prediction_BG,Bill_Welke_test$type)
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
Bill_Welke_Strikes = subset(Bill_Welke, Bill_Welke$type == "S")
Bill_Welke_Balls = subset(Bill_Welke, Bill_Welke$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Bill_Welke_Strikes$AdjustedCall = ifelse((Bill_Welke_Strikes$plate_x < 0.833 & Bill_Welke_Strikes$plate_x > -0.833) & (Bill_Welke_Strikes$plate_z > Bill_Welke_Strikes$sz_bot & Bill_Welke_Strikes$plate_z < Bill_Welke_Strikes$sz_top), 'S', 'B')
table(Bill_Welke_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Bill_Welke_Balls$AdjustedCall = ifelse((Bill_Welke_Balls$plate_x > 0.833 | Bill_Welke_Balls$plate_x < -0.833)|(Bill_Welke_Balls$plate_z < Bill_Welke_Balls$sz_bot | Bill_Welke_Balls$plate_z > Bill_Welke_Balls$sz_top),'B','S')
table(Bill_Welke_Balls$AdjustedCall)
# Merge to create new dataset
Bill_Welke_AdjustedCalls = rbind(Bill_Welke_Strikes,Bill_Welke_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Bill_Welke)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Bill_Welke_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Bill_Welke_AdjustedCalls$AdjustedCall))



