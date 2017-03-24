library(rpart)

nutrition = read.csv("/home/koolok/data_analysis/Projet/Ciqual2.csv", sep = ";")

total_score = 0

for (i in 1:50){
  n <- nrow(nutrition)
  
  train <- sort(sample(1:n, floor(n/2)))
  
  nutrition.train <- nutrition[train,]
  nutrition.test <- nutrition[-train,]
  
  class <- as.matrix(nutrition[c(2)])
  
  nutrition.rp <- rpart(class ~ . ,
                        data = nutrition[5:57],
                        subset = train,
                        method = "class",
                        parms = list(split = "information"),
                        maxsurrogate = 0,
                        cp = 0,
                        minsplit = 5,
                        minbucket =2
  )
  pred.rp <- predict(nutrition.rp,
                     newdata = nutrition[-train,],
                     type = "class")
  
  score = 0
  for(i in 1:(length(class[-train]))){
    if (class[-train][i] == pred.rp[i])
      score = score + 1
  }
  
  total_score = total_score + (score / nrow(nutrition.test))
}

total_score / 50
