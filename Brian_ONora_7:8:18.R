# 7/8/18 : Giants vs Cardinals : Brian O'Nora

# 187 pitches were called strikes/balls

# The robot-ump called 54 of those pitches as called strikes & 133 as balls

# Brian O'Nora called 62 of those pitches as called strikes & 125 as balls 

# Accuracy: 89%

Brian_ONora <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Brian_ONora_7:8:18.csv", header=TRUE)

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Brian_ONora)
names(Brian_ONora)
is.na(Brian_ONora)
colSums(is.na(Brian_ONora)) 
Brian_ONora = Brian_ONora[,colSums(is.na(Brian_ONora)) == 0] 
dim(Brian_ONora)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Brian_ONora = Brian_ONora[ , !(names(Brian_ONora) %in% drops)]
dim(Brian_ONora)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Brian_ONora_train = Brian_ONora[0:(0.8 * nrow(Brian_ONora)),]
dim(Brian_ONora_train)
prop.table(table(Brian_ONora_train$type))
Brian_ONora_test = Brian_ONora[(0.8*nrow(Brian_ONora)):nrow(Brian_ONora),]
dim(Brian_ONora_test)
prop.table(table(Brian_ONora_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Brian_ONora_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Brian_ONora_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Brian_ONora_test$type))

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Brian_ONora_Strikes = subset(Brian_ONora, Brian_ONora$type == "S")
Brian_ONora_Balls = subset(Brian_ONora, Brian_ONora$type == "B")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Brian_ONora_Strikes$AdjustedCall = ifelse((Brian_ONora_Strikes$plate_x < 0.833 & Brian_ONora_Strikes$plate_x > -0.833) & (Brian_ONora_Strikes$plate_z > Brian_ONora_Strikes$sz_bot & Brian_ONora_Strikes$plate_z < Brian_ONora_Strikes$sz_top), 'S', 'B')
table(Brian_ONora_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Brian_ONora_Balls$AdjustedCall = ifelse((Brian_ONora_Balls$plate_x > 0.833 | Brian_ONora_Balls$plate_x < -0.833)|(Brian_ONora_Balls$plate_z < Brian_ONora_Balls$sz_bot | Brian_ONora_Balls$plate_z > Brian_ONora_Balls$sz_top),'B','S')
table(Brian_ONora_Balls$AdjustedCall)

# Merge to create new dataset
Brian_ONora_AdjustedCalls = rbind(Brian_ONora_Strikes,Brian_ONora_Balls)

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Brian_ONora)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Brian_ONora_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Brian_ONora_AdjustedCalls$AdjustedCall))







