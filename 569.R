set.seed(569)
library("ReIns")
library("ggplot2")
library(fitdistrplus) 
library(plyr)
library(lgcp)
library(mgcv)

# Q1. If two students are connected in the projected bipartite network,
# how many same courses do they have?
Q1=read.csv("C:/Users/pc/Desktop/569 project/Q1.csv")
sameclasses=data.frame("number.of.same.classes"=1:7,"frequency"=c(50948,8287,1267,404,141,137,2),
                       "probability"=c(50948,8287,1267,404,141,137,2)/61186)

ggplot(sameclasses,aes(x=number.of.same.classes,y=frequency))+
  geom_bar(stat="identity",fill="lightblue",color="steelblue")+
  labs(title="frequency bar chart")+theme(plot.title=element_text(hjust=0.5))
  
ggplot(sameclasses,aes(x=number.of.same.classes,y=probability))+
  geom_bar(stat="identity",fill="lightblue",color="steelblue")+
  labs(title="probability bar chart")+theme(plot.title=element_text(hjust=0.5))

barplot(Q1$number.of.same.classes,main="barplot (number of same classes)")

ExpQQ(Q1$number.of.same.classes)
MeanExcess(Q1$number.of.same.classes,k=T)
MeanExcess(Q1$number.of.same.classes)

descdist(Q1$number.of.same.classes)
fit1=fitdistr(Q1$number.of.same.classes,"lognormal")
fit1
x1=rlnorm(10000,fit1[[1]][1],fit1[[1]][2])
x1_df=data.frame("index"=1:10000,"value"=x1)

ggplot()+
  geom_bar(data=sameclasses,aes(x=number.of.same.classes,y=probability),
           stat="identity",fill="lightblue",color="steelblue")+
  geom_density(data=x1_df,aes(value),color="blue")+
  labs(title="lognormal fit")+
  theme(plot.title=element_text(hjust=0.5))


sum=list()
for (a in 1:10){
  sum[a]=sum(Q1$number.of.same.classes[(-5999+6000*a):(6000*a)])}
sum_df=data.frame("Index"=1:10,"Sum"=unlist(sum))  


Q1_gam_poisson=gam(sum_df$Sum~s(sum_df$Index),family="poisson")
summary(Q1_gam_poisson)
x1_gam_poisson=predict(Q1_gam_poisson)
x1_gam_poisson_upper=x1_gam_poisson+0.003702*1.96
x1_gam_poisson_lower=x1_gam_poisson-0.003702*1.96

Q1_gam_negativebinomial=gam(sum_df$Sum~s(sum_df$Index),family=negbin(7000))
summary(Q1_gam_negativebinomial)
x1_gam_negativebinomial=predict(Q1_gam_negativebinomial)
x1_gam_negativebinomial_upper=x1_gam_negativebinomial+0.00529*1.96
x1_gam_negativebinomial_lower=x1_gam_negativebinomial-0.00529*1.96


ggplot()+
  geom_line(data=sum_df,aes(x=Index, y=Sum),size=1)+
  geom_line(aes(x=sum_df$Index,y=exp(x1_gam_poisson)),color="red",size=1)+
  geom_line(aes(x=sum_df$Index,y=exp(x1_gam_poisson_upper)),color="red",linetype=4)+
  geom_line(aes(x=sum_df$Index,y=exp(x1_gam_poisson_lower)),color="red",linetype=4)+
  geom_line(aes(x=sum_df$Index,y=exp(x1_gam_negativebinomial)),color="blue",size=1)+
  geom_line(aes(x=sum_df$Index,y=exp(x1_gam_negativebinomial_upper)),color="blue",linetype=4)+
  geom_line(aes(x=sum_df$Index,y=exp(x1_gam_negativebinomial_lower)),color="blue",linetype=4)+
  scale_colour_manual(values=c("red","blue"),labels=c("poisson","negative binomial"))+
  labs(title="GAM Fits")+theme(plot.title=element_text(hjust=0.5))










# 2. How many components can all 1861 students be divided into? 
Q2=read.csv("C:/Users/pc/Desktop/569 project/Q2.csv")

ggplot(data=Q2,aes(x=1:6,y=components_size))+
  geom_bar(stat="identity",fill="lightblue",color="steelblue")+
  geom_text(aes(label=components_size),size=3,position=position_stack(vjust=0.5))+
  xlab("index")+
  labs(title="component size bar chart")+theme(plot.title=element_text(hjust=0.5))












# Q3 What are the shortest paths from one student to every other student
# in the largest component?
Q3=read.csv("C:/Users/pc/Desktop/569 project/Q3.csv")
N_steps_students_statistics=data.frame("N_steps"=0:5,
                  "frequency"=c(1821,122366,1891654,1260198,39624,378),
                  "probability"=c(1821,122366,1891654,1260198,39624,378)/3316041)

ggplot(data=N_steps_students_statistics,aes(x=N_steps,y=frequency))+
  geom_bar(stat="identity",fill="lightblue",color="steelblue")+
  labs(title="frequency bar chart")+theme(plot.title=element_text(hjust=0.5))

ggplot(data=N_steps_students_statistics,aes(x=N_steps,y=probability))+
  geom_bar(stat="identity",fill="lightblue",color="steelblue")+
  labs(title="probability bar chart")+theme(plot.title=element_text(hjust=0.5))

barplot(Q3$N_steps[0:100000],main="barplot (number of steps)")

# ExpQQ(Q3$N_steps)
# MeanExcess(Q3$N_steps,k=T)
# MeanExcess(Q3$N_steps)

descdist(Q3$N_steps)
fit3=fitdistr(Q3$N_steps,"normal")
fit3
x3=rnorm(10000,fit3[[1]][1],fit3[[1]][2])
x3_df=data.frame("index"=1:10000,"value"=x3)

ggplot()+
  geom_bar(data=N_steps_students_statistics,
           aes(x=N_steps,y=probability),
           stat="identity",fill="lightblue",color="steelblue")+
  geom_density(data=x3_df,aes(value),color="blue")+
  labs(title="normal fit")+
  theme(plot.title=element_text(hjust=0.5))                    

sum_Q3=list()
for (a in 1:10){
  sum_Q3[a]=sum(Q3$N_steps[(-329999+330000*a):(330000*a)])}
sum_Q3_df=data.frame("Index"=1:10,"Sum"=unlist(sum_Q3))  


Q3_gam_poisson=gam(sum_Q3_df$Sum~s(sum_Q3_df$Index),family="poisson")
summary(Q3_gam_poisson)
x3_gam_poisson=predict(Q3_gam_poisson)
x3_gam_poisson_upper=x3_gam_poisson+0.000358*1.96
x3_gam_poisson_lower=x3_gam_poisson-0.000358*1.96

Q3_gam_negativebinomial=gam(sum_Q3_df$Sum~s(sum_Q3_df$Index),family=negbin(800000))
summary(Q3_gam_negativebinomial)
x3_gam_negativebinomial=predict(Q3_gam_negativebinomial)
x3_gam_negativebinomial_upper=x3_gam_negativebinomial+0.0005031*1.96
x3_gam_negativebinomial_lower=x3_gam_negativebinomial-0.0005031*1.96


ggplot()+
  geom_line(data=sum_Q3_df,aes(x=Index, y=Sum),size=1)+
  geom_line(aes(x=sum_Q3_df$Index,y=exp(x3_gam_poisson)),color="red",size=1)+
  geom_line(aes(x=sum_Q3_df$Index,y=exp(x3_gam_poisson_upper)),color="red",linetype=4)+
  geom_line(aes(x=sum_Q3_df$Index,y=exp(x3_gam_poisson_lower)),color="red",linetype=4)+
  geom_line(aes(x=sum_Q3_df$Index,y=exp(x3_gam_negativebinomial)),color="blue",size=1)+
  geom_line(aes(x=sum_Q3_df$Index,y=exp(x3_gam_negativebinomial_upper)),color="blue",linetype=4)+
  geom_line(aes(x=sum_Q3_df$Index,y=exp(x3_gam_negativebinomial_lower)),color="blue",linetype=4)+
  scale_colour_manual(values=c("red","blue"),labels=c("poisson","negative binomial"))+
  labs(title="GAM Fits")+theme(plot.title=element_text(hjust=0.5))










# Q4 Use three methods to assess the centrality of all the selected courses and 
# then design a rectified value based on the three calculated values.
Q4=read.csv("C:/Users/pc/Desktop/569 project/Q4.csv")
index=which(Q4$rectified.centrality==0)
Q4=Q4[-index,]


barplot(Q4$rectified.centrality,main="barplot (rectified centrality)")


ExpQQ(Q4$rectified.centrality)
MeanExcess(Q4$rectified.centrality)
MeanExcess(Q4$rectified.centrality,k=T)


ParetoQQ(Q4$rectified.centrality)
ParetoQQ_der(Q4$rectified.centrality)
ParetoQQ_der(Q4$rectified.centrality,k=T)

LognormalQQ(Q4$rectified.centrality)
LognormalQQ_der(Q4$rectified.centrality)
LognormalQQ_der(Q4$rectified.centrality,k=T)

WeibullQQ(Q4$rectified.centrality)
WeibullQQ_der(Q4$rectified.centrality)
WeibullQQ_der(Q4$rectified.centrality,k=T)



rectified.centrality_H=Hill(Q4$rectified.centrality,plot=TRUE,col="blue")
EPD(Q4$rectified.centrality,add=TRUE,col="orange",lty=2)
Hill.2oQV(Q4$rectified.centrality,add=TRUE,col="red",lty=3)
legend("topleft",c("Hill","EPD","Hill.2oQV"),col=c("blue","orange","red"),lty=1:3)



genQQ(Q4$rectified.centrality,gamma=rectified.centrality_H$gamma)
rectified.centrality_GH=genHill(Q4$rectified.centrality,gamma=rectified.centrality_H$gamma,
                            plot=TRUE,col="blue")
rectified.centrality_M=Moment(Q4$rectified.centrality,add=TRUE,lty=2)
legend("bottom",c("genHill","Moment"),col=c("blue","black"),lty=1:2)




p=0.01
Quant(Q4$rectified.centrality,gamma=rectified.centrality_H$gamma,p=p,plot=TRUE,col= "blue",lty=1,ylim=0:1)
QuantGH(Q4$rectified.centrality,gamma=rectified.centrality_GH$gamma,p=p,add=TRUE,col="red",lty=2,ylim=0:1)
QuantMOM(Q4$rectified.centrality,gamma=rectified.centrality_M$gamma,p=p,add=TRUE,col="black", lty=3,ylim=0:1)
legend("topright",c("Hill","genHill","Moment"),
       col=c("blue","red","black"),lty=1:3)


q=0.13
Return(Q4$rectified.centrality,gamma=rectified.centrality_H$gamma,q=q,
       plot=TRUE,col="blue",lty=1)
ReturnGH(Q4$rectified.centrality,gamma=rectified.centrality_GH$gamma,q=q,
         add=TRUE,col="red",lty=2)
ReturnMOM(Q4$rectified.centrality,gamma=rectified.centrality_M$gamma,q=q,add=TRUE,col="black",lty=3)
legend("bottomright", c("Hill","genHill","Moment"),col=c("blue","red","black"),lty=1:3)



MeanExcess(Q4$rectified.centrality)
abline(v=quantile(Q4$rectified.centrality,c(0.75,0.95)),lty=2)


spliceFit=SpliceFitPareto(Q4$rectified.centrality,const=c(0.75,0.95),M=3)
summary(spliceFit)
mefit=MEfit(p=c(0.101,0.899), shape=c(1,5), theta=0.035, M=2)
evtfit=EVTfit(gamma=c(0.866,0.223), endpoint=c(0.173,Inf))
splicefit=SpliceFit(const=c(0.75,0.95), trunclower=0, t=c(0.115,0.173), 
                       type=c("ME","TPa" ,"Pa"),
                       MEfit=mefit, EVTfit=evtfit)
summary(splicefit)

x=seq(0,1,0.01)
SpliceECDF(x,Q4$rectified.centrality,splicefit)

SplicePP(Q4$rectified.centrality,splicefit,log=TRUE)
SpliceQQ(Q4$rectified.centrality,splicefit)

Q4_sim=rSplice(1000, splicefit)
plot(Q4_sim,xlab="Index",ylab="Simulated Centrality")










# Q5 How many students every other student is exposed to?
Q5=read.csv("C:/Users/pc/Desktop/569 project/Q5.csv")
index=which(Q5$P_degree==0)
Q5=Q5[-index,]

barplot(Q5$P_degree,main="barplot (P_degree)")


ExpQQ(Q5$P_degree)
MeanExcess(Q5$P_degree)
MeanExcess(Q5$P_degree,k=T)


ParetoQQ(Q5$P_degree)
ParetoQQ_der(Q5$P_degree)
ParetoQQ_der(Q5$P_degree,k=T)

LognormalQQ(Q5$P_degree)
LognormalQQ_der(Q5$P_degree)
LognormalQQ_der(Q5$P_degree,k=T)

WeibullQQ(Q5$P_degree)
WeibullQQ_der(Q5$P_degree)
WeibullQQ_der(Q5$P_degree,k=T)


P_degree_H=Hill(Q5$P_degree,plot=TRUE,col="blue",ylim=0:1)
EPD(Q5$P_degree,add=TRUE,col="orange",lty=2)
Hill.2oQV(Q5$P_degree,add=TRUE,col="red",lty=3)
legend("topleft",c("Hill","EPD","Hill.2oQV"),col=c("blue","orange","red"),lty=1:3)


genQQ(Q5$P_degree,gamma=P_degree_H$gamma)
P_degree_GH=genHill(Q5$P_degree,gamma=P_degree_H$gamma,
                                plot=TRUE,col="blue",ylim=-0.5:0.5)
P_degree_M=Moment(Q5$P_degree,add=TRUE,lty=2)
legend("top",c("genHill","Moment"),col=c("blue","black"),lty=1:2)



p=0.01
Quant(Q5$P_degree,gamma=P_degree_H$gamma,p=p,plot=TRUE,col= "blue",lty=1,ylim=c(50,500))
QuantGH(Q5$P_degree,gamma=P_degree_GH$gamma,p=p,add=TRUE,col="red",lty=2)
QuantMOM(Q5$P_degree,gamma=P_degree_M$gamma,p=p,add=TRUE,col="black", lty=3)
legend("topleft",c("Hill","genHill","Moment"),
       col=c("blue","red","black"),lty=1:3)


q=100
Return(Q5$P_degree,gamma=P_degree_H$gamma,q=q,
       plot=TRUE,col="blue",lty=1)
ReturnGH(Q5$P_degree,gamma=P_degree_GH$gamma,q=q,
         add=TRUE,col="red",lty=2)
ReturnMOM(Q5$P_degree,gamma=P_degree_M$gamma,q=q,add=TRUE,col="black",lty=3)
legend("bottomright", c("Hill","genHill","Moment"),col=c("blue","red","black"),lty=1:3)



MeanExcess(Q5$P_degree)
abline(v=quantile(Q5$P_degree,c(0.85,0.99)),lty=2)


spliceFit=SpliceFitPareto(Q5$P_degree,const=c(0.85,0.99),M=2)
summary(spliceFit)
mefit=MEfit(p=c(0.052,0.948), shape=c(2,7), theta=10.736, M=2)
evtfit=EVTfit(gamma=c(0.146,0.086), endpoint=c(125,Inf))
splicefit=SpliceFit(const=c(0.85,0.99), trunclower=0, t=c(94,125), 
                    type=c("ME","TPa" ,"Pa"),
                    MEfit=mefit, EVTfit=evtfit)
summary(splicefit)

x=seq(0,200,0.1)
SpliceECDF(x,Q5$P_degree,splicefit)

SplicePP(Q5$P_degree,splicefit,log=TRUE)
SpliceQQ(Q5$P_degree,splicefit)

Q5_sim=rSplice(1000, splicefit)
plot(Q5_sim,xlab="Index",ylab="Simulated Value")










# Q6 What's the number of students per class?
Q6=read.csv("C:/Users/pc/Desktop/569 project/Q6.csv")

barplot(Q6$classes_degree,main="barplot (classes_degree)")

ExpQQ(Q6$classes_degree)
MeanExcess(Q6$classes_degree)
MeanExcess(Q6$classes_degree,k=T)


ParetoQQ(Q6$classes_degree)
ParetoQQ_der(Q6$classes_degree)
ParetoQQ_der(Q6$classes_degree,k=T)

LognormalQQ(Q6$classes_degree)
LognormalQQ_der(Q6$classes_degree)
LognormalQQ_der(Q6$classes_degree,k=T)

WeibullQQ(Q6$classes_degree)
WeibullQQ_der(Q6$classes_degree)
WeibullQQ_der(Q6$classes_degree,k=T)


classes_degree_H=Hill(Q6$classes_degree,plot=TRUE,col="blue",ylim=c(-0.7,1.2))
EPD(Q6$classes_degree,add=TRUE,col="orange",lty=2)
Hill.2oQV(Q6$classes_degree,add=TRUE,col="red",lty=3)
legend("topleft",c("Hill","EPD","Hill.2oQV"),col=c("blue","orange","red"),lty=1:3)


genQQ(Q6$classes_degree,gamma=classes_degree_H$gamma)
classes_degree_GH=genHill(Q6$classes_degree,gamma=classes_degree_H$gamma,
                    plot=TRUE,col="blue",ylim=c(-1,3))
classes_degree_M=Moment(Q6$classes_degree,add=TRUE,lty=2)
legend("top",c("genHill","Moment"),col=c("blue","black"),lty=1:2)



p=0.05
Quant(Q6$classes_degree,gamma=classes_degree_H$gamma,p=p,plot=TRUE,col= "blue",lty=1,ylim=c(10,100))
QuantGH(Q6$classes_degree,gamma=classes_degree_GH$gamma,p=p,add=TRUE,col="red",lty=2)
QuantMOM(Q6$classes_degree,gamma=classes_degree_M$gamma,p=p,add=TRUE,col="black", lty=3)
legend("topleft",c("Hill","genHill","Moment"),
       col=c("blue","red","black"),lty=1:3)


q=20
Return(Q6$classes_degree,gamma=classes_degree_H$gamma,q=q,
       plot=TRUE,col="blue",lty=1)
ReturnGH(Q6$classes_degree,gamma=classes_degree_GH$gamma,q=q,
         add=TRUE,col="red",lty=2)
ReturnMOM(Q6$classes_degree,gamma=classes_degree_M$gamma,q=q,add=TRUE,col="black",lty=3)
legend("bottomright", c("Hill","genHill","Moment"),col=c("blue","red","black"),lty=1:3)



MeanExcess(Q6$classes_degree)
abline(v=quantile(Q6$classes_degree,c(0.8,0.96)),lty=2)


spliceFit=SpliceFitPareto(Q6$classes_degree,const=c(0.8,0.96),M=2)
summary(spliceFit)
mefit=MEfit(p=c(0.32,0.68), shape=c(2,12), theta=1.34, M=2)
evtfit=EVTfit(gamma=c(0.8,0.216), endpoint=c(28,Inf))
splicefit=SpliceFit(const=c(0.8,0.96), trunclower=0, t=c(19,28), 
                    type=c("ME","TPa" ,"Pa"),
                    MEfit=mefit, EVTfit=evtfit)
summary(splicefit)

x=seq(0,100,0.1)
SpliceECDF(x,Q6$classes_degree,splicefit)

SplicePP(Q6$classes_degree,splicefit,log=TRUE)
SpliceQQ(Q6$classes_degree,splicefit)

Q6_sim=rSplice(1000, splicefit)
plot(Q6_sim,xlab="Index",ylab="Simulated Value")










# 7. What's the flow rate in one classroom on a given day?
Q7=read.csv("C:/Users/pc/Desktop/569 project/Q7.csv")
maxflowrate_df=data.frame("Room"=c("Boyer Hall, Room AMP","Brubaker Hall, Room 304",
       "Boyer Hall, Room AMP","Boyer Hall, Room 205","Boyer Hall, Room AMP"),"Date"=c(
       "Monday","Tuesday","Wednesday","Thursday","Friday"),"maxflowrate"=c(69,34,92,34,68))
maxflowrate_df$Date=factor(maxflowrate_df$Date,level=c("Monday","Tuesday","Wednesday","Thursday","Friday"))
ggplot(data=maxflowrate_df,aes(x=Date,y=maxflowrate))+
  geom_bar(stat="identity",fill="lightblue",color="steelblue")+
  geom_text(aes(label=Room),size=3,position=position_stack(vjust=0.5))+
  geom_text(aes(label=maxflowrate),size=5,position=position_stack(vjust=1))+
  labs(title="maximum flow rate bar chart")+theme(plot.title=element_text(hjust=0.5))










# 8. What's the potential flow rate in the dining hall on a given day?
Q8=read.csv("C:/Users/pc/Desktop/569 project/Q8.csv")
Q8$X=factor(Q8$X,level=c("Monday","Tuesday","Wednesday","Thursday","Friday"))
ggplot(data=Q8,aes(x=X,y=dininghall_flowrate))+
  geom_bar(stat="identity",fill="lightblue",color="steelblue")+
  geom_text(aes(label=dininghall_flowrate),size=5,position=position_stack(vjust=0.5),color="blue")+
  labs(x="Date",title="potential flow rate in the dining hall")+theme(plot.title=element_text(hjust=0.5))

