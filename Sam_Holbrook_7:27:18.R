#7/27/18: Giants vs Brewers : Sam_Holbrook

#117 pitches were called strikes/balls

#The robot-ump called 35 of those pitches as called strikes & 82 as balls

#Sam_Holbrook called 40 of those pitches as called strikes & 77 as balls 

#Accuracy: 94%

Sam_Holbrook <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Sam_Holbrook_7:27:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Sam_Holbrook)
names(Sam_Holbrook)
is.na(Sam_Holbrook)
colSums(is.na(Sam_Holbrook)) 
Sam_Holbrook = Sam_Holbrook[,colSums(is.na(Sam_Holbrook)) == 0] 
dim(Sam_Holbrook)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Sam_Holbrook = Sam_Holbrook[ , !(names(Sam_Holbrook) %in% drops)]
# Sam_Holbrook = Sam_Holbrook[keep]
dim(Sam_Holbrook)
Sam_Holbrook

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Sam_Holbrook_train = Sam_Holbrook[0:(0.8 * nrow(Sam_Holbrook)),]
dim(Sam_Holbrook_train)
prop.table(table(Sam_Holbrook_train$type))
Sam_Holbrook_test = Sam_Holbrook[(0.8*nrow(Sam_Holbrook)):nrow(Sam_Holbrook),]
dim(Sam_Holbrook_test)
prop.table(table(Sam_Holbrook_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Sam_Holbrook_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Sam_Holbrook_test, type = 'class')

confusionMatrix(table(prediction_BG, Sam_Holbrook_test$type))
confusionMatrix(prediction_BG,Sam_Holbrook_test$type)
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
Sam_Holbrook_Strikes = subset(Sam_Holbrook, Sam_Holbrook$type == "S")
Sam_Holbrook_Balls = subset(Sam_Holbrook, Sam_Holbrook$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Sam_Holbrook_Strikes$AdjustedCall = ifelse((Sam_Holbrook_Strikes$plate_x < 0.833 & Sam_Holbrook_Strikes$plate_x > -0.833) & (Sam_Holbrook_Strikes$plate_z > Sam_Holbrook_Strikes$sz_bot & Sam_Holbrook_Strikes$plate_z < Sam_Holbrook_Strikes$sz_top), 'S', 'B')
table(Sam_Holbrook_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Sam_Holbrook_Balls$AdjustedCall = ifelse((Sam_Holbrook_Balls$plate_x > 0.833 | Sam_Holbrook_Balls$plate_x < -0.833)|(Sam_Holbrook_Balls$plate_z < Sam_Holbrook_Balls$sz_bot | Sam_Holbrook_Balls$plate_z > Sam_Holbrook_Balls$sz_top),'B','S')
table(Sam_Holbrook_Balls$AdjustedCall)
# Merge to create new dataset
Sam_Holbrook_AdjustedCalls = rbind(Sam_Holbrook_Strikes,Sam_Holbrook_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Sam_Holbrook)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Sam_Holbrook_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Sam_Holbrook_AdjustedCalls$AdjustedCall))




