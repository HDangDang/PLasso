#训练样本数train,测试样本数test,p值数据来源样本数ptrain
#真实变量系数分布参数：均值betamu，方差betasigma2<-10，截距项cons
#变量数v，真实变量比例betaper
#信噪比snr
#变量基本相关性rho
#实验次数experiment
simulate<-function(train,test,ptrain,
                   v,betaper,
                   snr,
                   betamu,betasigma2,cons,
                   rho,
                   experiment){
  library(glmnet)
  betanumber<-v*betaper
  #总样本数
  n<-train+test
  #变量方差
  variablemu<-rep(0,v)
  variablesigma2<-matrix(NA,nrow = v,ncol = v)
  for(row in 1:v){
    for(col in 1:v){
      if(row!=col){
        variablesigma2[row,col]<-rho^abs(row-col)
      }else{
        variablesigma2[row,col]<-rho^abs(row-col)
      }
    }
  }
  #实验结果存储
  simu<-data.frame(order=1:experiment,#实验顺序
                   #真实变量个数
                   truenumber=numeric(experiment),
                   #lasso筛选变量数
                   lassonumber=numeric(experiment),
                   #lasso筛选的真实变量数
                   lassotrue=numeric(experiment),
                   #lasso估计系数的偏差平方和
                   lassobeta2=numeric(experiment),
                   #lasso估计的测试集偏差平方和
                   lassopre2=numeric(experiment),
                   #adaptive lasso筛选变量数
                   lnpnumber=numeric(experiment),
                   #adaptive lasso筛选的真实变量数
                   lnptrue=numeric(experiment),
                   #adaptive lasso估计系数的偏差平方和
                   lnpbeta2=numeric(experiment),
                   #adaptive lasso估计的测试集偏差平方和
                   lnppre2=numeric(experiment),
                   yt=numeric(experiment)
  )
  
  set.seed(1111)
  #实验
  for(i in 1:experiment){
    print(i)
    
    simu$truenumber<-betanumber
    #p值数据生成
    pbeta<-rep(0,v)
    pbeta[1:betanumber]<-rnorm(betanumber,betamu,sqrt(betasigma2))
    pbeta<-as.matrix(pbeta,nrow=v)
    pX<-mvrnorm(ptrain,variablemu,variablesigma2)
    pY0<-cons+pX%*%pbeta
    pepsilons <- var(pY0) / snr
    pY<- pY0 + rnorm(ptrain,0,sqrt(pepsilons))
    pdata<-cbind(pY,as.data.frame(pX))
    
    #模拟数据生成
    beta<-rep(0,v)
    beta[1:betanumber]<-rnorm(betanumber,betamu,sqrt(betasigma2))
    beta<-as.matrix(beta,nrow=v)
    truevariable<-which(beta!=0)
    coef<-c(cons,beta)
    #生成X与Y
    X<-mvrnorm(n,variablemu,variablesigma2)
    Y0<-cons+X%*%beta
    epsilons <- var(Y0) / snr
    Y<- Y0 + rnorm(n,0,sqrt(epsilons))
    data<-cbind(Y,as.data.frame(X))
    
    #训练集与测试集
    trainlist <-sample(n,train)
    xtest<-as.matrix(X[-trainlist, ]) 
    ytest<-as.matrix(Y[-trainlist])
    xtrain<-X[trainlist,]
    ytrain<-Y[trainlist]
    
    #lasso
    lasso<-cv.glmnet(xtrain,ytrain,nfolds = 10)
    plot(lasso)
    lassocoef<-as.matrix(coef(lasso,lasso$lambda.min))
    lassochoose<-which(lassocoef!=0)
    simu$lassonumber[i]<-length(lassochoose)
    simu$lassotrue[i]<-length(intersect(lassochoose,truevariable))
    simu$lassobeta2[i]<-sum((lassocoef-coef)^2)/v
    predictlasso<-predict(lasso,newx = xtest,s='lambda.min')
    simu$lassopre2[i]<-sum((predictlasso-ytest)^2)/test
    
    #Plasso
    #计算p值
    pvalue<-data.frame(variable=numeric(v),p=numeric(v))
    #每个模型单独算p值
    for(m in 1:(ncol(pdata)-1)){
      variable<-colnames(pdata)[m+1]
      datalm<-pdata[,c(1,m+1)]
      lm<-summary(lm(pY~.,data = datalm))
      p<-lm$coefficients[2,4]
      pvalue$variable[m]<-variable
      pvalue$p[m]<-p
    }
    
    pvalue$lnp<-log(pvalue[,2])
    pvalue$flnp<-(-1)/pvalue$lnp
    
    #寻找使得mse最小的参数
    mse<-9999999999
    
    for(k in 1:100){
      print(k)
      plassotry<-cv.glmnet(xtrain,ytrain,
                           nfolds = 10,
                           penalty.factor=(pvalue$flnp)^(k/100))
      
      if(plassotry$cvm[plassotry$lambda==plassotry$lambda.min]<mse){
        lnpcoef<-as.matrix(coef(plassotry,s='lambda.min'))
        lnpchoose<-which(lnpcoef!=0)
        ytbest<-k
        predictlnp<-predict(plassotry,newx = xtest)
        mse<-plassotry$cvm[plassotry$lambda==plassotry$lambda.min]
      }else{
        next
      }
    }
    simu$lnpnumber[i]<-length(lnpchoose)
    simu$lnptrue[i]<-length(intersect(lnpchoose,truevariable))
    simu$lnpbeta2[i]<-sum((lnpcoef-coef)^2)/v
    simu$lnppre2[i]<-sum((predictlnp-ytest)^2)/test
    simu$yt[i]<-ytbest
  }
  return(simu)
}
