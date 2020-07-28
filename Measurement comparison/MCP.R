## Open file directory
scriptPath <- function() {
  getSrcDirectory(scriptPath);
}
path <- scriptPath()
setwd(path)

#Import data tables
merra<-read.table('MERRA_N55.500_E12.000_1995-2015.txt', skip = 10, header = FALSE, sep ='\t')
colnames(merra)<-c("Date [UTC]","wsp [m/s]","dir [deg]","temp [°C]","press [kPa]")

riso<-read.table('riso mast_44_2013-Exported_hourly.txt', skip = 12, header = FALSE, sep =',')
colnames(riso)<-c("Date [UTC]","row [-]","wsp [m/s]","dir [deg]")



#Initialize empty data frame
matches<-data.frame(row.names = c("c1","c2"))

#Searching matching data. Matching data have the same date. Slightly optimized not to search for past data.
last_j<-1
for (i in 1:length(riso$'Date [UTC]')){ #Iterating over all riso data
  for (j in last_j:length(merra$'Date [UTC]')){ #Iterating over merra data, starting from the last known match
    if (merra$'Date [UTC]'[j]==riso$'Date [UTC]'[i]){
      matches<-rbind(matches,c(riso$'wsp [m/s]'[i],merra$'wsp [m/s]'[j]))
      last_j<-j
      break
    }
  }
}
colnames(matches)<-c("riso","merra")

#Present results and draw linear regression line

plot(x=matches$merra, y=matches$riso, col=4, main="Concurrent measurements at different sites", xlab="Merra wsp (ref) [m/s]", ylab="Riso wsp (site) [m/s]", pch=20) 

fit<-lm(y~x,data=data.frame(x=matches$merra,y=matches$riso))
lines(x=0:20, y=(fit$coefficients[1]+fit$coefficients[2]*(0:20)), col=1, lwd=3, lty=1)

legend(0, 25, legend=c("Data", "Fit line (y~x)"),col=c("blue", "black"), lty=1, cex=1)
print(summary(fit))