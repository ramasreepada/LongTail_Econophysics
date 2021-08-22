train_set = as.matrix(read.csv("Results/Train_Set_With_InjectedRatings_Length6_0.05.csv",sep=','))
train_set = as.matrix(train_set[,-1])

# train_items = as.matrix(unique(train_set[,2]))

test_set = as.matrix(read.csv("Test_Set.csv",sep=','))
test_set = as.matrix(test_set[,-1])

# test_items = as.matrix(unique(test_set[,2]))

predicted_set = matrix(0,nrow=nrow(test_set),ncol=3)
predicted_set[,1] = test_set[,1]
predicted_set[,2] = test_set[,2]

mean_of_users = matrix(0,nrow=nrow(as.matrix(unique(train_set[,1]))),ncol=2)
mean_of_users[,1] = as.matrix(unique(train_set[,1]))

for(i in 1:nrow(mean_of_users))
{
  mean_of_users[i,2] = mean(train_set[which(train_set[,1]==mean_of_users[i,1]),3])
}
#########


function_cosine_sim <- function(current_item, neighbor_item,mean_of_users)
{
  current_item_instances = matrix(train_set[which(train_set[,2] == current_item),] ,ncol=3)
  
  if(current_item != neighbor_item)
  {
    
    neighbor_item_instances = matrix(train_set[which(train_set[,2] == neighbor_item),] ,ncol=3)
    
    common_users = as.matrix(intersect(current_item_instances[,1],neighbor_item_instances[,1]))
    
    if(nrow(common_users>=1))
    {
      mean_of_common_users = matrix(mean_of_users[mean_of_users[,1]%in% common_users,],ncol=2)
      
      mean_of_common_users = as.matrix(mean_of_common_users[order(mean_of_common_users[,1]),2])
      
      current_item_subset = matrix(current_item_instances[which(current_item_instances[,1] %in% common_users),],ncol=3)
      neighbor_item_subset = matrix(neighbor_item_instances[which(neighbor_item_instances[,1] %in% common_users),],ncol=3)
      
      if(nrow(neighbor_item_subset) != length(unique(neighbor_item_subset[,1])))
      {
        neighbor_item_subset = matrix(neighbor_item_subset[!duplicated(neighbor_item_subset[,(1:2)]),],ncol=3)
      }
      
      if(nrow(current_item_subset) != length(unique(current_item_subset[,1])))
      {
        current_item_subset = matrix( current_item_subset[!duplicated(current_item_subset[,(1:2)]),],ncol=3)
      }
      
      
      
      item_x = as.matrix(current_item_subset[order(current_item_subset[,1],decreasing=FALSE),3])
      item_y = as.matrix(neighbor_item_subset[order(neighbor_item_subset[,1],decreasing=FALSE),3])
      
      deno = (sqrt((sum((item_x-mean_of_common_users)*(item_x-mean_of_common_users)))*(sum((item_y-mean_of_common_users)*(item_y-mean_of_common_users)))))
      num = sum((item_x-mean_of_common_users)*(item_y-mean_of_common_users))
      
      if(deno>0)
      {
        return(num/deno);
      }
      
      else
      {
        return(0);
      }
    }
    
    else
    {
      return(0);
    }
  }
  
  else
  {
    return(0);
  }
  
  
}




####################################
for(i in 1:nrow(test_set))
  # for(i in 1:50)
{
  current_user = test_set[i,1]
  current_item = test_set[i,2]
  
  #current user rated_items
  items_rated_by_currentUser = matrix(train_set[which(train_set[,1]==current_user),],ncol=3)
  cosine_similarity = t(as.matrix(c(0,0,0)))
  
  for(j in 1:nrow(items_rated_by_currentUser))
  {
    cos_value = function_cosine_sim(current_item,items_rated_by_currentUser[j,2],mean_of_users)
    
    if(cos_value > 0)
    {
      cosine_similarity = matrix(rbind(cosine_similarity,c(current_item, items_rated_by_currentUser[j,2],cos_value)),ncol=3)
    }
    
  }
  
  if(nrow(cosine_similarity) > 1)
  {
    # print(i)
    cosine_similarity = matrix(cosine_similarity[-1,],ncol=3)
    
    cosine_similarity = matrix(cosine_similarity[order(cosine_similarity[,2],decreasing=FALSE),],ncol=3)
    
    #  similar_items_rated_values
    similar_items_rated_values = matrix(items_rated_by_currentUser[which(items_rated_by_currentUser[,2] %in% cosine_similarity[,2]),],ncol=3)
    
    similar_items_rated_values = matrix(similar_items_rated_values[order(similar_items_rated_values[,2],decreasing = FALSE),],ncol=3)
    
    # 
    # mean_values = matrix(0,nrow=nrow(similar_items_rated_values),ncol=1)
    # 
    # for(l in 1:nrow(similar_items_rated_values))
    # {
    #   mean_values[l] = mean(train_set[which(train_set[,2]==similar_items_rated_values[l,2]),3])
    # }
    
    
    similar_items_rated_values = as.matrix(similar_items_rated_values[order(similar_items_rated_values[,2],decreasing = FALSE),3])
    
    # predicted_set[i,3] = (mean(train_set[which(train_set[,2]==current_item),3])) + (sum(cosine_similarity[,3]*(similar_items_rated_values-mean_values)))/(sum(cosine_similarity[,3]))
    predicted_set[i,3] = (sum(cosine_similarity[,3]*(similar_items_rated_values)))/(sum(cosine_similarity[,3]))
  }
  
  # else
  # {
  #   predicted_set[i,3] = mean(train_set[which(train_set[,2]==current_item),3])
  # }
  
}

write.csv(predicted_set,"Results/PredictionSet_IBCF_ACS_injected__TailLength6_0.05.csv")
