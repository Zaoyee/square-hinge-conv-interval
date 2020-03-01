
setwd("~/Documents/square-hinge-conv-interval - data Original/square-hinge-conv-interval/R-test")
library(penaltyLearning)
#data1 <- read.csv('inputs.csv.xz')[,-1]
#targets <- read.csv('CopyOfoutputs.csv')[,-1]
#folds <- read.csv('CopyOffolds.csv')[,-1]

#data1 <- read.csv('featured100.csv')[,-1]
# dd <- read.csv('data_wky_dd.csv')[,-1]

data1 <- read.csv('featured1000.csv')[,-1]
dd <- read.csv('data_label_F_processed.csv')[,-1]

data1 <- as.matrix(as.data.frame(lapply(data1, as.numeric)))
dd <- as.matrix(as.data.frame(lapply(dd, as.numeric)))
targets <- dd[,(ncol(dd)-4):(ncol(dd)-3)]
folds <- as.integer(dd[,ncol(dd)])


tst_idx = 1

#data1 <- as.matrix(as.data.frame(lapply(data1, as.numeric)))
#targets <- as.matrix(as.data.frame(lapply(targets, as.numeric)))
data2 <- data1[,colSums(!is.na(data1)) > 0]

rowidx = ((targets[,1] == -Inf) & (targets[,2] == Inf))
data2 = data2[!rowidx,]
targets = targets[!rowidx,]
folds = folds[!rowidx]

#train_idx = (folds != tst_idx)
#data3 = data2[train_idx,]
#targets3 = targets[train_idx,]
#folds3 = folds[train_idx]

fit.cv <- penaltyLearning::IntervalRegressionCV(feature.mat = data2,
                                                target.mat = targets,
                                                fold.vec = folds)

acc = NULL
for(idx in 1:6){
  tst_idx = (folds == idx)
  pred <- predict(fit.cv, data2[tst_idx,])
  out <- targets[tst_idx,]
  num = sum(ifelse(pred>out[,1],ifelse(pred<out[,2], 1, 0),0))
  acc[idx] = num/nrow(out)
}
acc
plot(acc)
