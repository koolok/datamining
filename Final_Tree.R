library(rpart)

nutrition = read.csv("/home/koolok/data_analysis/Projet/Ciqual2.csv", sep = ";")

class <- as.matrix(nutrition[c(2)])

nutrition.rp <- rpart(class ~ . ,
                      data = nutrition[5:57],
                      method = "class",
                      parms = list(split = "information"),
                      maxsurrogate = 0,
                      cp = 0,
                      minsplit = 5,
                      minbucket =2
)

summary(nutrition.rp)