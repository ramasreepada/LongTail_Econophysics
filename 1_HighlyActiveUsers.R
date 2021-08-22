train_set = as.matrix(read.csv("Train_Set.csv", sep=','))
train_set = as.matrix(train_set[,-1])

test_set = as.matrix(read.csv("Test_Set.csv", sep=','))
test_set = as.matrix(test_set[,-1])

# item_cardinality = as.matrix(read.csv("ItemCardinality_on_TrainSet.csv", sep=','))
# item_cardinality = as.matrix(item_cardinality[,-1])

item_cardinality = matrix(0,nrow=nrow(as.matrix(unique(train_set[,2]))), ncol=2)
item_cardinality[,1] = as.matrix(unique(train_set[,2]))

for(i in 1:nrow(item_cardinality))
{
  current_item = item_cardinality[i,1]
  
  item_cardinality[i,2] = nrow(as.matrix(which(train_set[,2] == current_item)))
}


item_cardinality = item_cardinality[order(item_cardinality[,2], decreasing = TRUE),]
# write.csv(item_cardinality,"ItemCardinality_on_TrainSet.csv")

################

item_cardinality = as.matrix(read.csv("ItemCardinality_on_TrainSet.csv",sep = ','))
# first column is user id, second column is number of items rated, third column 
# is number of popluar items rated
# limits are obtained from the cardinality plots
upperlimit = 5000
lowerlimit = 20

popular_items = as.matrix(item_cardinality[which(item_cardinality[,2] >= 5000),1])
tail_items = as.matrix(item_cardinality[which(item_cardinality[,2] <= 20),1])

highly_active_users = matrix(0, nrow=nrow(as.matrix(unique(train_set[,1]))),ncol=2)

unique_users = as.matrix(unique(train_set[,1]))

for(i in 1:nrow(unique_users))
{
  
  current_user_rated_items = as.matrix(train_set[which(train_set[,1] == unique_users[i]),2]) 
  
  popular_rated_items = as.matrix(intersect(current_user_rated_items,popular_items))
  
  highly_active_users[i,1] = unique_users[i]
  highly_active_users[i,2] = nrow(popular_rated_items)
}

write.csv(highly_active_users,"Highly_Active_Users.csv")
