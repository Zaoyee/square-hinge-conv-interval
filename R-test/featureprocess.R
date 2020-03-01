library(penaltyLearning)
data <- read.csv('data_label_F_processed.csv')[,-1]
data <- data[,(-ncol(data)+7:-ncol(data))]

result = matrix(nrow = nrow(data), ncol = 365)
for(i in 1:nrow(data)){
  result[i,] = penaltyLearning::featureVector(data.vec = as.numeric(data[i,]))  
}

write.csv(result, "./featured1000.csv")
