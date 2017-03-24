library(rpart)
library(parallel)

nutrition = read.csv("/home/koolok/Documents/master/s2/data_analysis/Projet/Ciqual2.csv", sep = ";")

test_validation = function(i){
  score = 0
  nutrition.train <- nutrition[-i,]
  nutrition.test <- nutrition[i,]
  
  class.train <- as.matrix(nutrition.train[c(2)])
  class.test <- as.matrix(nutrition.test[c(2)])
  
  nutrition.rp <- rpart(class.train ~ . ,
                        data = nutrition.train[5:57],
                        method = "class",
                        parms = list(split = "information"),
                        maxsurrogate = 0,
                        cp = 0,
                        minsplit = 5,
                        minbucket =2
  )
  
  pred.rp <- predict(nutrition.rp,
                     newdata = nutrition.test[5:57],
                     type = "class")
  print(pred.rp)
  print(class.test[1])
  if (pred.rp == class.test[1])
    score = score + 1
  return(score)
}

no_cores <- detectCores()
cl <- makeCluster(no_cores, type="FORK")

scores = parSapply(cl, 1:nrow(nutrition), test_validation)
