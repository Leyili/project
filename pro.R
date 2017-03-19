setwd("~/Desktop/")
library(sas7bdat)
student=read.sas7bdat('test.sas7bdat')
student$PTNUMBER=as.factor(student$PTNUMBER)

group1=student[which(student[,1]=='Normal'),]
group2=student[which(student[,1]=='Preecl'),]
group3=student[which(student[,1]=='SGA'),]
group=rbind(group1,group3)

fm1=lmer(lnsve~time+time2+time3+time4+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group)
coef1=fixef(fm1)
fm2=lmer(lnsve~time+time2+time3+time4+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group2)
coef2=fixef(fm2)

library(lme4)
library(rootSolve)
##boot ???????????????
tn=as.numeric()
tn1=as.numeric()
tn2=as.numeric()
for( j in 1:20){
  sam=sample(student$PTNUMBER,137,replace = T)
  sam1=as.vector(sam)
  id1=NULL
  for(i in 1:137){
    id=which(student$PTNUMBER==sam1[i])
    id1=c(id1,id)
  }
  student1=student[id1,]
  
  group1=student1[which(student1[,1]=='Normal'),]
  group2=student1[which(student1[,1]=='Preecl'),]
  group3=student1[which(student1[,1]=='SGA'),]
  
  fm1=lmer((lnsve+1)~time+time2+time3+time4+time5+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group1)
  coef1=fixef(fm1)
  fm2=lmer((lnsve+1)~time+time2+time3+time4+time5+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group2)
  coef2=fixef(fm2)
  fm3=lmer((lnsve+1)~time+time2+time3+time4+time5+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group3)
  coef3=fixef(fm3)
  
  delt=coef1 - coef3
  delt1=coef1-coef2
  delt2=coef2-coef3
  tt<-function(t) 1*delt[1]+t*delt[2]+t^2*delt[3]+t^3*delt[4]+t^4*delt[5]+t^5*delt[6]
  tt1<-function(t) 1*delt1[1]+t*delt1[2]+t^2*delt1[3]+t^3*delt1[4]+t^4*delt1[5]+t^5*delt1[6]
  tt2<-function(t) 1*delt2[1]+t*delt2[2]+t^2*delt2[3]+t^3*delt2[4]+t^4*delt2[5]+t^5*delt2[6]
  tn[j]=max(uniroot.all(tt,c(15,37)))
  tn1[j]=max(uniroot.all(tt1,c(15,37)))
  tn2[j]=max(uniroot.all(tt2,c(15,37)))
  
}


## get the hessian matrix
#vcov(fm)
#solve for the true point quantile

quantile(tn[tn>0],c(0.025,0.975))
quantile(tn1[tn1>0],c(0.025,0.975))
quantile(tn2[tn2>0],c(0.025,0.975))

##boot ???????????????
tvalue=as.numeric()
tvalue1=as.numeric()
tvalue2=as.numeric()
for( j in 1:20){
  sam=sample(student$PTNUMBER,137,replace = T)
  sam1=as.vector(sam)
  id1=NULL
  for(i in 1:137){
    id=which(student$PTNUMBER==sam1[i])
    id1=c(id1,id)
  }
  student1=student[id1,]
  
  group1=student1[which(student1[,1]=='Normal'),]
  group2=student1[which(student1[,1]=='Preecl'),]
  group3=student1[which(student1[,1]=='SGA'),]
  
  fm1=lmer((lnsve+1)~time+time2+time3+time4+time5+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group1)
  coef1=fixef(fm1)
  fm2=lmer((lnsve+1)~time+time2+time3+time4+time5+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group2)
  coef2=fixef(fm2)
  fm3=lmer((lnsve+1)~time+time2+time3+time4+time5+(1|PTNUMBER)+(0+time+time2|PTNUMBER),group3)
  coef3=fixef(fm3)
  
  t1=seq(from=8,to=40,by=0.1)
  difu=as.numeric()
  for( i in 1:321){
    t=t1[i]  
    L=c(1,t,t^2,t^3,t^4)
    var1=(L%*%vcov(fm2)%*%L)
    var2=(L%*%vcov(fm1)%*%L)
    ei=sum((coef2-coef1)*L)
    difu[i]=ei+1.96*sqrt(var1+var2)
  }
  aa=rbind(t1[1:321],dif)
  tvalue[j]=max(t1[which(abs(aa[2,])<0.05)])
  
  dif=as.numeric()
  for( i in 1:321){
    t=t1[i]  
    L=c(1,t,t^2,t^3,t^4,t^5)
    var1=(L%*%vcov(fm3)%*%L)
    var2=(L%*%vcov(fm1)%*%L)
    ei=sum((coef3-coef1)*L)
    dif[i]=ei-1.96*sqrt(var1+var2)
  }

  aa=rbind(t1[1:321],dif)
  tvalue1[j]=max(t1[which(abs(aa[2,])<0.05)])
  
  dif=as.numeric()
  for( i in 1:321){
    t=t1[i]  
    L=c(1,t,t^2,t^3,t^4,t^5)
    var1=(L%*%vcov(fm3)%*%L)
    var2=(L%*%vcov(fm2)%*%L)
    ei=sum((coef3-coef2)*L)
    dif[i]=ei+1.96*sqrt(var1+var2)
  }
  
  
  aa=rbind(t1[1:321],dif)
  tvalue2[j]=max(t1[which(abs(aa[2,])<0.05)])
}

quantile(tvalue[tvalue>0],c(0.025,0.975))
quantile(tvalue1[tvalue1>0],c(0.025,0.975))
quantile(tvalue2[tvalue2>0],c(0.025,0.975))


dd=data.frame(lower=difl,true=dif,upper=difu,time=t1)
d1=melt(dd,id='time')
ggplot(data=d1, aes(x=time, y=value, colour=variable)) + geom_line()
