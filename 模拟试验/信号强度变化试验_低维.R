library(ggplot2)
####v50，snr1####
simu1<-simulate(train=100,test=50,ptrain=100,
                v=50,betaper=0.1,
                snr=1,
                betamu=0,betasigma2=10,cons=10,
                rho=0,
                experiment=30)
result1<-data.frame(Method=c("Lasso","PLasso"),
                    set=1,
                    VariableNum=c(mean(simu$lassonumber),mean(simu$lnpnumber)),
                    RealInclu=c(mean(simu$lassotrue/simu$truenumber),
                                mean(simu$lnptrue/simu$truenumber)),
                    FakeInclu=c(
                      mean((v-betanumber-simu$lassonumber+simu$lassotrue)/(v-betanumber)),
                      mean((v-betanumber-simu$lnpnumber+simu$lnptrue)/(v-betanumber))),
                    Bias2=c(mean(simu$lassobeta2),mean(simu$lnpbeta2)),
                    Pre2=c(mean(simu$lassopre2),mean(simu$lnppre2)))

resultall<-result1

####v50，snr=5####
simu5<-simulate(train=100,test=50,ptrain=100,
                v=50,betaper=0.1,
                snr=5,
                betamu=0,betasigma2=10,cons=10,
                rho=0.3,
                experiment=30)
result3<-data.frame(Method=c("Lasso","PLasso"),
                    set=5,
                    VariableNum=c(mean(simu$lassonumber),mean(simu$lnpnumber)),
                    RealInclu=c(mean(simu$lassotrue/simu$truenumber),
                                mean(simu$lnptrue/simu$truenumber)),
                    FakeInclu=c(
                      mean((v-betanumber-simu$lassonumber+simu$lassotrue)/(v-betanumber)),
                      mean((v-betanumber-simu$lnpnumber+simu$lnptrue)/(v-betanumber))),
                    Bias2=c(mean(simu$lassobeta2),mean(simu$lnpbeta2)),
                    Pre2=c(mean(simu$lassopre2),mean(simu$lnppre2)))

resultall<-rbind(resultall,result3)

####v50，snr=10####
simu10<-simulate(train=100,test=50,ptrain=100,
                 v=50,betaper=0.1,
                 snr=10,
                 betamu=0,betasigma2=10,cons=10,
                 rho=0.3,
                 experiment=30)
result10<-data.frame(Method=c("Lasso","PLasso"),
                     set=10,
                     VariableNum=c(mean(simu$lassonumber),mean(simu$lnpnumber)),
                     RealInclu=c(mean(simu$lassotrue/simu$truenumber),
                                 mean(simu$lnptrue/simu$truenumber)),
                     FakeInclu=c(
                       mean((v-betanumber-simu$lassonumber+simu$lassotrue)/(v-betanumber)),
                       mean((v-betanumber-simu$lnpnumber+simu$lnptrue)/(v-betanumber))),
                     Bias2=c(mean(simu$lassobeta2),mean(simu$lnpbeta2)),
                     Pre2=c(mean(simu$lassopre2),mean(simu$lnppre2)))

resultall<-rbind(resultall,result10)

####v50绘图####
data<-resultall
data$set<-as.factor(data$set)
variable<-data[,c(1,2,3)]
realInclu<-data[,c(1,2,4)]
fakeInclu<-data[,c(1,2,5)]
Bias2<-data[,c(1,2,6)]
Pre2<-data[,c(1,2,7)]
p1<-ggplot(data=realInclu,aes(x=set,y=RealInclu,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("snr")+ylab("正确的真实变量比例")+
  theme(legend.position = "top")

p2<-ggplot(data=fakeInclu,aes(x=set,y=FakeInclu,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("snr")+ylab("正确的无关变量比例")+
  theme(legend.position = "top")

p3<-ggplot(data=Bias2,aes(x=set,y=Bias2,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("snr")+ylab("平均系数估计偏差平方")+
  theme(legend.position = "top")

p4<-ggplot(data=Pre2,aes(x=set,y=Pre2,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("snr")+ylab("平均预测误差平方")+
  theme(legend.position = "top")

p5<-ggplot(data=variable,aes(x=set,y=VariableNum,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("snr")+ylab("筛选变量个数")+
  theme(legend.position = "top")

p1
p2
p3
p4
p5
