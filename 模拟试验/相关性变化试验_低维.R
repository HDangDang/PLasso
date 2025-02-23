library(ggplot2)
####v50，相关系数0####
simu0<-simulate(train=100,test=50,ptrain=100,
                v=50,betaper=0.1,
                snr=10,
                betamu=0,betasigma2=10,cons=10,
                rho=0,
                experiment=30)
result0<-data.frame(Method=c("Lasso","PLasso"),
                    set=0,
                    VariableNum=c(mean(simu$lassonumber),mean(simu$lnpnumber)),
                    RealInclu=c(mean(simu$lassotrue/simu$truenumber),
                                mean(simu$lnptrue/simu$truenumber)),
                    FakeInclu=c(
                      mean((v-betanumber-simu$lassonumber+simu$lassotrue)/(v-betanumber)),
                      mean((v-betanumber-simu$lnpnumber+simu$lnptrue)/(v-betanumber))),
                    Bias2=c(mean(simu$lassobeta2),mean(simu$lnpbeta2)),
                    Pre2=c(mean(simu$lassopre2),mean(simu$lnppre2)))

resultall<-result0

####v50，相关系数0.3####
simu3<-simulate(train=100,test=50,ptrain=100,
                v=50,betaper=0.1,
                snr=10,
                betamu=0,betasigma2=10,cons=10,
                rho=0.3,
                experiment=30)
result3<-data.frame(Method=c("Lasso","PLasso"),
                    set=0.3,
                    VariableNum=c(mean(simu$lassonumber),mean(simu$lnpnumber)),
                    RealInclu=c(mean(simu$lassotrue/simu$truenumber),
                                mean(simu$lnptrue/simu$truenumber)),
                    FakeInclu=c(
                      mean((v-betanumber-simu$lassonumber+simu$lassotrue)/(v-betanumber)),
                      mean((v-betanumber-simu$lnpnumber+simu$lnptrue)/(v-betanumber))),
                    Bias2=c(mean(simu$lassobeta2),mean(simu$lnpbeta2)),
                    Pre2=c(mean(simu$lassopre2),mean(simu$lnppre2)))

resultall<-rbind(resultall,result3)

####v50，相关系数0.6####
simu6<-simulate(train=100,test=50,ptrain=100,
                v=50,betaper=0.1,
                snr=10,
                betamu=0,betasigma2=10,cons=10,
                rho=0.6,
                experiment=30)
result6<-data.frame(Method=c("Lasso","PLasso"),
                    set=0.6,
                    VariableNum=c(mean(simu$lassonumber),mean(simu$lnpnumber)),
                    RealInclu=c(mean(simu$lassotrue/simu$truenumber),
                                mean(simu$lnptrue/simu$truenumber)),
                    FakeInclu=c(
                      mean((v-betanumber-simu$lassonumber+simu$lassotrue)/(v-betanumber)),
                      mean((v-betanumber-simu$lnpnumber+simu$lnptrue)/(v-betanumber))),
                    Bias2=c(mean(simu$lassobeta2),mean(simu$lnpbeta2)),
                    Pre2=c(mean(simu$lassopre2),mean(simu$lnppre2)))

resultall<-rbind(resultall,result6)

####v50，相关系数0.9####
simu9<-simulate(train=100,test=50,ptrain=100,
                v=50,betaper=0.1,
                snr=10,
                betamu=0,betasigma2=10,cons=10,
                rho=0.9,
                experiment=30)
result9<-data.frame(Method=c("Lasso","PLasso"),
                    set=0,
                    VariableNum=c(mean(simu$lassonumber),mean(simu$lnpnumber)),
                    RealInclu=c(mean(simu$lassotrue/simu$truenumber),
                                mean(simu$lnptrue/simu$truenumber)),
                    FakeInclu=c(
                      mean((v-betanumber-simu$lassonumber+simu$lassotrue)/(v-betanumber)),
                      mean((v-betanumber-simu$lnpnumber+simu$lnptrue)/(v-betanumber))),
                    Bias2=c(mean(simu$lassobeta2),mean(simu$lnpbeta2)),
                    Pre2=c(mean(simu$lassopre2),mean(simu$lnppre2)))

resultall<-rbind(resultall,result9)

####低维绘图####
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
  xlab("基础相关系数")+ylab("正确的真实变量比例")+
  theme(legend.position = "top")

p2<-ggplot(data=fakeInclu,aes(x=set,y=FakeInclu,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("基础相关系数")+ylab("正确的无关变量比例")+
  theme(legend.position = "top")

p3<-ggplot(data=Bias2,aes(x=set,y=Bias2,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("基础相关系数")+ylab("平均系数估计偏差平方")+
  theme(legend.position = "top")

p4<-ggplot(data=Pre2,aes(x=set,y=Pre2,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("基础相关系数")+ylab("平均预测误差平方")+
  theme(legend.position = "top")

p5<-ggplot(data=variable,aes(x=set,y=VariableNum,group=Method)) + 
  geom_line(aes(colour=Method)) + 
  geom_point(size=6,aes(shape=Method,colour=Method)) + 
  xlab("基础相关系数")+ylab("筛选变量个数")+
  theme(legend.position = "top")

p1
p2
p3
p4
p5
