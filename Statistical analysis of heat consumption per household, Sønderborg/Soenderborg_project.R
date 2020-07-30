# Open file directory
scriptPath <- function() {
  getSrcDirectory(scriptPath);
}
path <- scriptPath()
setwd(path)
# Load the data into the variable D
D <- read.table("soenderborg1_data.csv", sep=";", header=TRUE, as.is=TRUE)

# a)
D
summary(D)

# b) change data input and title as appropriate
hist(D$Q1, prob=TRUE,col="red",main="Heat consumption, House 1",xlab="Heat consumption [kW/day]",breaks=seq(0,8, by=0.5),ylim=c(0,1))

# c)
D$t <- as.Date(x=D$t, format="%Y-%m-%d") ##creates a vector of dates
plot(D$t,D$Q1,xlim=as.Date(c("2008-10-02","2010-10-02")),ylim=c(0,10),xlab="year",ylab="Heat [kW/day]",type="l",main="Heat consumption, time series")
lines(D$t, D$Q2, col=2)
lines(D$t, D$Q3, col=3)
lines(D$t, D$Q4, col=4)
legend("topright", legend = paste("House",c(1,2,3,4)), lty=1, col=1:4)

# d)
Dsel <- subset(D, "2010-01-01" <= t & t < "2010-3-01") ## to set the time period

boxplot(Dsel[ ,c("Q1","Q2","Q3","Q4")], ylab="Heat consumption [kW/day]",col=c("white","red","green","blue"),names=c("House 1","House 2","House 3","House 4"),las=2,main='Heat consumption during winter 2010')

# e)
# to compile the table (change input as needed)
mean(Dsel$Q1,na.rm=TRUE)
var(Dsel$Q1,na.rm=TRUE)
sd(Dsel$Q1,na.rm=TRUE)
quantile(Dsel$Q1,c(0.25,0.5,0.75),na.rm=TRUE)

# f) change function input as appropriate
qqnorm((Dsel$Q1),main="Validation of the assumption of normal distr., house 1",xlab="z-quantile", ylab="Heat")
qqline((Dsel$Q1))
points(x=qnorm(p=c(0.25,0.75)), y=quantile(Dsel$Q1,probs=c(0.25,0.75),na.rm=TRUE),pch=3, col="red")

xsim <- rnorm(60, mean(Dsel$Q1,na.rm=TRUE), sd(Dsel$Q1,na.rm=TRUE))
# Do the q-q normal plot with inbuilt functions
qqnorm(xsim)
qqline(xsim)

# g-h-i)
CIu1=mean(Dsel$Q1,na.rm=TRUE)+qt(0.975,sum(!is.na(Dsel$Q1))-1)*sd(Dsel$Q1,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q1)))
CIl1=mean(Dsel$Q1,na.rm=TRUE)-qt(0.975,sum(!is.na(Dsel$Q1))-1)*sd(Dsel$Q1,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q1)))
CIu2=mean(Dsel$Q2,na.rm=TRUE)+qt(0.975,sum(!is.na(Dsel$Q2))-1)*sd(Dsel$Q2,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q2)))
CIl2=mean(Dsel$Q2,na.rm=TRUE)-qt(0.975,sum(!is.na(Dsel$Q2))-1)*sd(Dsel$Q2,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q2)))
CIu3=mean(Dsel$Q3,na.rm=TRUE)+qt(0.975,sum(!is.na(Dsel$Q3))-1)*sd(Dsel$Q3,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q3)))
CIl3=mean(Dsel$Q3,na.rm=TRUE)-qt(0.975,sum(!is.na(Dsel$Q3))-1)*sd(Dsel$Q3,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q3)))
CIu4=mean(Dsel$Q4,na.rm=TRUE)+qt(0.975,sum(!is.na(Dsel$Q4))-1)*sd(Dsel$Q4,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q4)))
CIl4=mean(Dsel$Q4,na.rm=TRUE)-qt(0.975,sum(!is.na(Dsel$Q4))-1)*sd(Dsel$Q4,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q4)))

M = c(mean(Dsel$Q1,na.rm=TRUE),mean(Dsel$Q2,na.rm=TRUE),mean(Dsel$Q3,na.rm=TRUE),mean(Dsel$Q4,na.rm=TRUE))
L = c(CIl1,CIl2,CIl3,CIl4)
U = c(CIu1,CIu2,CIu3,CIu4)

require(plotrix)
plotCI(M, ui=U, li=L,main="Confidence Interval for mean of heat cons. winter 2010",xlab="House",ylab="Confidence Interval [kW/day]",col=1:4,axes=FALSE)
axis(side=2, at=seq(4.2, 5.6, by=0.2))
axis(side=1, at=c(0:4))
box()

# OR (change input as needed)
t.test(Dsel$Q1,conf.level=0.95)

# j)
tobs=(mean(Dsel$Q1,na.rm=TRUE)-2.38)/(sd(Dsel$Q1,na.rm=TRUE)/sqrt(sum(!is.na(Dsel$Q1))))
pvalue=2*(1-pt(abs(tobs),sum(!is.na(Dsel$Q1))-1))
# confront with t.test
t.test(Dsel$Q1,conf.level=0.95,mu=2.38)

# k)
tobs=((mean(Dsel$Q1,na.rm=TRUE)-mean(Dsel$Q2,na.rm=TRUE))-0)/sqrt(var(Dsel$Q1,na.rm=TRUE)/sum(!is.na(Dsel$Q1))+var(Dsel$Q2,na.rm=TRUE)/sum(!is.na(Dsel$Q2)))
v=(var(Dsel$Q1,na.rm=TRUE)/sum(!is.na(Dsel$Q1))+var(Dsel$Q2,na.rm=TRUE)/sum(!is.na(Dsel$Q2)))^2/((var(Dsel$Q1,na.rm=TRUE)/sum(!is.na(Dsel$Q1)))^2/(sum(!is.na(Dsel$Q1))-1)+(var(Dsel$Q2,na.rm=TRUE)/sum(!is.na(Dsel$Q2)))^2/(sum(!is.na(Dsel$Q2))-1))
pvalue=2*(1-pt(abs(tobs),v))
# confront with t.test
t.test(Dsel$Q1,Dsel$Q2)

# m)
newQ1=ifelse(D$G=="NA",NA,D$Q1)
newG=ifelse(D$Q1=="NA",NA,D$G)
Sxy=1/(sum(!is.na(newQ1))-1)*sum((newQ1-mean(newQ1,na.rm=TRUE))*(newG-mean(newG,na.rm=TRUE)),na.rm=TRUE)
Sx=sd(newQ1,na.rm=TRUE)
Sy=sd(newG,na.rm=TRUE)
corr=Sxy/(Sx*Sy)
plot(newG,newQ1,main="Heat consumption plotted against global radiation",xlab="Global radiation",ylab="Heat consumption")
abline(lm(newQ1 ~ newG),col="2")

# for testing purposes
cor(D[,c("Q1","G")], use="pairwise.complete.obs")