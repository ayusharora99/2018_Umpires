#8/27/18: Giants vs A' : Ben May

#150 pitches were called strikes/balls

#The robot-ump called 48 of those pitches as called strikes & 102 as balls

#Ben May called 55 of those pitches as called strikes & 95 as balls 

#Accuracy: 86%

Ben_May <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Ben_May_7:15:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Ben_May)
names(Ben_May)
is.na(Ben_May)
colSums(is.na(Ben_May)) 
Ben_May = Ben_May[,colSums(is.na(Ben_May)) == 0] 
dim(Ben_May)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Ben_May = Ben_May[ , !(names(Ben_May) %in% drops)]
# Ben_May = Ben_May[keep]
dim(Ben_May)
Ben_May

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Ben_May_train = Ben_May[0:(0.8 * nrow(Ben_May)),]
dim(Ben_May_train)
prop.table(table(Ben_May_train$type))
Ben_May_test = Ben_May[(0.8*nrow(Ben_May)):nrow(Ben_May),]
dim(Ben_May_test)
prop.table(table(Ben_May_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Ben_May_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Ben_May_test, type = 'class')

confusionMatrix(table(prediction_BG, Ben_May_test$type))
confusionMatrix(prediction_BG,Ben_May_test$type)
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
Ben_May_Strikes = subset(Ben_May, Ben_May$type == "S")
Ben_May_Balls = subset(Ben_May, Ben_May$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Ben_May_Strikes$AdjustedCall = ifelse((Ben_May_Strikes$plate_x < 0.833 & Ben_May_Strikes$plate_x > -0.833) & (Ben_May_Strikes$plate_z > Ben_May_Strikes$sz_bot & Ben_May_Strikes$plate_z < Ben_May_Strikes$sz_top), 'S', 'B')
table(Ben_May_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Ben_May_Balls$AdjustedCall = ifelse((Ben_May_Balls$plate_x > 0.833 | Ben_May_Balls$plate_x < -0.833)|(Ben_May_Balls$plate_z < Ben_May_Balls$sz_bot | Ben_May_Balls$plate_z > Ben_May_Balls$sz_top),'B','S')
table(Ben_May_Balls$AdjustedCall)
# Merge to create new dataset
Ben_May_AdjustedCalls = rbind(Ben_May_Strikes,Ben_May_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Ben_May)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Ben_May_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Ben_May_AdjustedCalls$AdjustedCall))



