library(openxlsx)
library(glmnet)
library(ggplot2)
library(dplyr)
library(reshape2)
data_1<-read.xlsx("C:\\Users\\DangDang\\Desktop\\毕业论文终稿\\MUSK1.xlsx")
data_2<-read.xlsx("C:\\Users\\DangDang\\Desktop\\MUSK2.xlsx")
#第1、2列为名称,删除
data1<-data_1[,-c(1,2)]
data2<-data_2[,-c(1,2)]
#####data2用来计算p值#####
v<-ncol(data2)-1
pvalue<-data.frame(variable=numeric(v),p=numeric(v))
for(i in 1:v){
  variable<-colnames(data2)[i]
  datalm<-data2[,c(i,167)]
  glm<-summary(glm(class~.,data=datalm))
  p<-glm$coefficients[2,4]
  pvalue$variable[i]<-variable
  pvalue$p[i]<-p
}
pvalue$lnp<-log(pvalue[,2])
pvalue$flnp<-(-1)/pvalue$lnp

result<-data.frame(PLassovari=numeric(30),#PLasso变量数
                   PLassoclass=numeric(30),#PLasso分类正确率
                   Lassovari=numeric(30),#Lasso变量数
                   Lassoclass=numeric(30))#Lasso分类正确率

#总样本数n
n<-nrow(data1)

set.seed(1111)
for(e in 1:30){
  print(e)
  #data1拆分训练集和测试集
  trainlist<-sample(n,n*0.8)
  train<-data1[trainlist,]
  test<-data1[-trainlist,]
  
  xtrain<-as.matrix(train[,-167])
  ytrain<-train$class
  
  xtest<-as.matrix(test[,-167])
  ytest<-test$class
  
  ####寻找使得分类错误率最小的参数####
  error<-1
  
  for(k in 1:100){
    print(k)
    plassotry<-cv.glmnet(xtrain,ytrain,nfolds = 10,
                         family='binomial',type.measure = 'class',
                         penalty.factor=(pvalue$flnp)^(k/100))
    predictlnp<-predict(plassotry,newx=xtest,
                        s='lambda.min',type='class')
    wrong<-length(which(predictlnp!=ytest))/length(predictlnp)
    if(wrong<error){
      lnpcoef<-as.matrix(coef(plassotry,s='lambda.min'))
      lnpchoose<-which(lnpcoef!=0)
      error<-wrong 
    }else{
      next
    }
  }
  result$PLassoclass[e]<-1-error
  result$PLassovari[e]<-length(lnpchoose)
  lasso<-cv.glmnet(xtrain,ytrain,nfolds=10,
                   family='binomial',type.measure = 'class')
  prelasso<-predict(lasso,newx=xtest,
                    s='lambda.min',type='class')
  lassocoef<-as.matrix(coef(lasso,s='lambda.min'))
  lassochoose<-which(lassocoef!=0)
  result$Lassoclass[e]<-1-length(which(prelasso!=ytest))/length(prelasso)
  result$Lassovari[e]<-length(lassochoose)
}

####图####
#分类正确率
class<-data.frame(order=1:30,
                  PLasso=result$PLassoclass,
                  Lasso=result$Lassoclass)

longresult<-melt(
  class,                       #待转换的数据集名称
  id.vars=c("order"),  #要保留的主字段
  variable.name="method",         #转换后的分类字段名称（维度）
  value.name="分类正确率"             #转换后的度量值名称
)

ggplot(data=longresult, aes(x=method,y=分类正确率))+
  geom_boxplot(aes(fill=method))+
  theme(legend.position = 'none', 
        axis.title.y = element_text(face = 'bold'),
        axis.text.x = element_text(face = 'bold'))+
  labs(x="模型",y="分类正确率", main = "")





####相关性热力图####
library(corrplot)
cor<-cor(data1[,-167])
corrplot(cor,method = "color",
         tl.pos="n",diag=FALSE,
         addCoefasPercent=TRUE)
