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

tail_length = 5
tail_items = as.matrix(item_cardinality[which(item_cardinality[,2] <= tail_length),1])
popular_items = as.matrix(item_cardinality[which(item_cardinality[,2] >=500),1])


injected_instances = t(as.matrix(c(0,0,0)))

for(i in 1:nrow(tail_items))
{
  current_item = tail_items[i]

  picked_item = sample(popular_items,1)
  
  picked_item_instances = matrix(train_set[which(train_set[,2]==picked_item),],ncol=3)
  
  
  # randomly select x% of users from final_active_users
  picked_item_users = picked_item_instances[,1]
  
  reject_users = matrix(train_set[which(train_set[,2]==current_item),1],ncol=1)
  reject_users = rbind(reject_users, matrix(test_set[which(test_set[,2]==current_item),1],ncol=1))
  
  accept_users = setdiff(picked_item_instances[,1],reject_users)
  picked_item_instances_final = matrix(picked_item_instances[which(picked_item_instances[,1] %in% accept_users),],ncol=3)
  
  sample_indexes = sample(1:nrow(picked_item_instances_final),size=(floor(per_users*nrow(picked_item_instances_final))), replace = FALSE)
  
  selected_instances = matrix(picked_item_instances[sample_indexes,],ncol=3)
  
  selected_instances[,2] = current_item
  
  injected_instances = as.matrix(rbind(injected_instances, selected_instances))
}

injected_instances = as.matrix(injected_instances[-1,])



train_set_injected = as.matrix(rbind(train_set,injected_instances))

# write.csv(train_set_injected,"Train_Set_With_InjectedRatings_0.05.csv")