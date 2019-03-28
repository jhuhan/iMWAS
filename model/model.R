######model: learn the model on training data and output selected features and coefficents
######---------Input: 
######----------------trainX, trainY
######---------Return: 
######----------------result: selected features
model<-function(trainX,trainY){
  require(glmnet)
  set.seed(2018)
  require(e1071)
  if(nrow(trainX)!=length(trainY)) stop("Number of observations is differerent")
  
  ####impute missing value using mean value
  if(sum(is.na(trainX))>0){
    colmean<-colMeans(trainX,na.rm=TRUE)
    id<-apply(trainX,2,anyNA)
    id<-which(id==TRUE)
    for(j in 1:length(id)){
      trainX[is.na(trainX[,id[j]])==TRUE,id[j]]=colmean[id[j]]
    }
  } 

  ####model training  
      a <- seq(0, 1, 0.1)
      search <- foreach(ai = a, .combine = rbind) %dopar% {
        cv <-
          cv.glmnet(
            as.matrix(trainX),
            as.matrix(trainY),
            nfold = 10,
            type.measure = "mse",
            paralle = TRUE,
            alpha = ai
          )
        gridresult<-data.frame(cv$lambda[cv$nzero>0],cv$cvm[cv$nzero>0],cv$nzero[cv$nzero>0])
        colnames(gridresult)<-c('lambda','cvm','nzero')
        optlambda<-gridresult$lambda[gridresult$cvm == min(gridresult$cvm)]
        data.frame(
          cvm = min(gridresult$cvm),
          lambda.1se = optlambda,
          alpha = ai,
          nzero = gridresult$nzero[gridresult$lambda == optlambda]
        )
      } #########select best tuning parameter alpha using cv
      
      cv3 <- search[search$cvm == min(search$cvm),]
      yfit = glmnet(
        as.matrix(trainX),
        as.matrix(trainY),
        lambda = cv3$lambda.1se,
        alpha = cv3$alpha
      ) #########fit the model under best tuning parameter
      
      idf <- coef(yfit)
      selectf <- data.frame( ########save the selected features
 	 features = idf@Dimnames[[1]][which(idf != 0)], 
        coefs = idf [which(idf != 0)]
      )  
  
  return(selectf)
}
