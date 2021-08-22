library(recosystem)  

r = Reco()
# train_set = as.matrix(read.csv("Train_Set.csv", sep=','))
train_set = as.matrix(read.csv("Train_Set.csv", sep=','))
train_set = as.matrix(train_set[,-1])

test_set = as.matrix(read.csv("Test_Set.csv", sep=','))
test_set = as.matrix(test_set[,-1])


train_data = data_memory(train_set[,1],train_set[,2],train_set[,3])
test_data = data_memory(test_set[,1],test_set[,2],test_set[,3])

r$train(train_data, opts = list(dim = 200,                        # 4
                      costp_l1 = 0, costp_l2 = 0.01,   # 5
                      costq_l1 = 0, costq_l2 = 0.01,   # 6
                      niter = 500,                      # 7
                      nthread = 4))

x = 0
x <- as.matrix(r$predict(test_data, out_memory()))

# write.csv(x,"PredictionResults_Uninjected.csv")
write.csv(x,"Results/PredictionResults_uninjected.csv")
