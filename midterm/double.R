##### model assessment OUTER CV (with model selection INNER CV as part of model-fitting) #####
fulldata.out = df_factor
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the cross-validation splits 
groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels
set.seed(8)
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 

allpredictedCV.out = rep(NA,n.out)
for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  traindata.out = df_factor[!groupj.out,]
  testdata.out = df_factor[groupj.out,]
  ### entire model-fitting process ###
    fulldata.in = traindata.out  # only input the data used to fit the model
    x.in = model.matrix(CAMLEVEL ~ AGE + LAB, data=fulldata.in)[,-(1:4)]
    y.in = fulldata.in[, 'CAMLEVEL']
    k.in = 10 
    n.in = dim(fulldata.in)[1]
    groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
#    set.seed(8)   # do not reset seed for each internal loop
    cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
    #KNN cross-validation
   all_best_knn <- rep(NA, n.in)
   for (i in 1:k.in) { 
       groupi.in = (cvgroups.in == i)
       traindata.in = fulldata.in[!groupi.in, ]
       testdata.in = fulldata.in[groupi.in, ]
       best_k = get_best_k(traindata.in, testdata.in, 'CAMLEVEL', c('AGE', 'LAB'))
   }
  ### resulting in bestkKNN ###
  KNNtrainfit.out = knn(traindata.out[, c('AGE', 'LAB')], testdata.out[, c('AGE', 'LAB')], traindata.out[, 'CAMLEVEL'], k=best_k)
  allpredictedCV.out[groupj.out] = KNNtrainfit.out
}
#assessment
y.out = fulldata.out$CAMLEVEL
CV.out = get_error_rate(allpredictedCV.out, y.out)
CV.out