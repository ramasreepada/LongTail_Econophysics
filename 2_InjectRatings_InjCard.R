per_users = 0.05
train_set = as.matrix(read.csv("Train_Set.csv", sep=','))
train_set = as.matrix(train_set[,-1])

test_set = as.matrix(read.csv("Test_Set.csv", sep=','))
test_set = as.matrix(test_set[,-1])

item_cardinality = as.matrix(read.csv("ItemCardinality_on_TrainSet.csv", sep=','))
item_cardinality = item_cardinality[,-1]

highly_active_users = as.matrix(read.csv("Highly_Active_Users.csv", sep=','))
highly_active_users = highly_active_users[,-1]
#sorted highly active users
highly_active_users = highly_active_users[order(highly_active_users[,2], decreasing = TRUE),]

final_active_users = as.matrix(highly_active_users[which(highly_active_users[,2] >= 150 ),1])

tail_length = 6
tail_items = as.matrix(item_cardinality[which(item_cardinality[,2] <= tail_length),1])
length(tail_items)

injected_instances = t(as.matrix(c(0,0,0)))

for(i in 1:nrow(tail_items))
{
  current_item = tail_items[i]
  
  # randomly select 10% of users from final_active_users
  current_active_users = as.matrix(setdiff(final_active_users,train_set[train_set[,2]==current_item,1]))
 
  current_active_users = as.matrix(setdiff(current_active_users,test_set[test_set[,2]==current_item,1]))
  
  sample_indexes = sample(1:nrow(current_active_users),size=(floor(per_users*nrow(current_active_users))), replace = FALSE)
  
  selected_users = as.matrix(final_active_users[sample_indexes])
  
  injected_instances = as.matrix(rbind(injected_instances, cbind(selected_users,current_item,(3.5+1/(item_cardinality[which(item_cardinality[,1] == current_item),2])))))
}

injected_instances = as.matrix(injected_instances[-1,])


train_set_injected = as.matrix(rbind(train_set,injected_instances))

write.csv(train_set_injected,"Results/Train_Set_With_InjectedRatings_Length10_0.05.csv")
