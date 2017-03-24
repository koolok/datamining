nutrition = read.csv("/home/koolok/data_analysis/Projet/Ciqual2.csv", sep = ";")

summary(nutrition[c(2)])

#impossible to read
cor = cor(nutrition[c(-1,-2,-3,-4)])

#Smart analyze correlation
for(i in 5:60){
  for(j in (i+1):61){
    if (j!=i){
      t = cor(nutrition[c(i,j)])
      
      if ((0.8 > t[2,1] && t[2,1] > 0.5) || t[2,1] < -0.5)
        pairs(nutrition[c(i,j)], main = paste("Corelation = ", t[2,1]))
    }
  }
}

