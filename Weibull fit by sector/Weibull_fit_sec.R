require(pracma)
require(weibullness)

## Open file directory
scriptPath <- function() {
  getSrcDirectory(scriptPath);
}
path <- scriptPath()
setwd(path)

#Import data table
riso<-read.table('riso mast_44_2013-Exported_hourly.txt', skip = 12, header = FALSE, sep =',')
colnames(riso)<-c("Date [UTC]","row [-]","wsp [m/s]","dir [deg]")

#Analyzing wind speed sample
sol=weibullness::weibull.mle(riso$`wsp [m/s]`, threshold=0)
exp_density<-density(riso$`wsp [m/s]`)

#One way ks test 
ks_result<-ks.test(unique(riso$`wsp [m/s]`),'pweibull',shape=sol$shape,scale=sol$scale)
if (ks_result$p.value > 0.05){
  null_hp<-'accepts'
}else{
  null_hp<-'rejects'
}
ks_text<-paste('ks test',null_hp,'null hp. alpha=0.05',sep=' ')

plot(x=seq(0,25,0.01),y=dweibull(seq(0,25,0.01),shape=sol$shape,scale=sol$scale),type='l',ylim=c(0,0.2),main='Experimental vs Fitted Weibull pdf, all sectors',xlab='wsp [m/s]',ylab='Density')
lines(x=exp_density$x,y=exp_density$y,col=2)
legend(8,0.2,legend=c('Fitted','Experimental',ks_text),col=c('black','red',''),lty=c(1,1,0))

##Separating data into 12 sectors
sectors_edges<-seq(-15,345,30)
sec_list<-vector(mode='list',length=12)
names(sec_list)<-paste('sec_',num2str(1:12,fmt=0),sep='')

for (i in 1:length(riso$'dir [deg]')){
  #Setting up angle
  if (riso$'dir [deg]'[i] > 345){
    riso$'dir [deg]'[i]<-riso$'dir [deg]'[i]-360
  }
  #Appending to appropriate sector
  for (j in 1:(length(sectors_edges)-1)){
    if (riso$'dir [deg]'[i] > sectors_edges[j] && riso$'dir [deg]'[i] <= sectors_edges[j+1]){
      sec_list[[j]]<-append(sec_list[[j]],riso$'wsp [m/s]'[i])
      break
    }
  }
}

#Performing Weibull fit on sectorized wind speeds
WBL_par_sec<-vector(mode='list',length=12)

for (i in 1:length(sec_list)){
  #Analyzing wind speed sample
  WBL_par_sec[[i]]=weibullness::weibull.mle(sec_list[[i]], threshold=0)
  exp_density<-density(unique(sec_list[[i]]))
  #One way ks test 
  ks_result<-ks.test(unique(sec_list[[i]]),'pweibull',shape=WBL_par_sec[[i]]$shape,scale=WBL_par_sec[[i]]$scale)
  
  #Plotting
  main_title<-paste('Experimental vs Fitted Weibull pdf, ',names(sec_list)[i],sep='')
  plot(x=seq(0,25,0.01),y=dweibull(seq(0,25,0.01),shape=WBL_par_sec[[i]]$shape,scale=WBL_par_sec[[i]]$scale),type='l',ylim=c(0,0.2),main=main_title,xlab='wsp [m/s]',ylab='Density')
  lines(x=exp_density$x,y=exp_density$y,col=2)
  if (ks_result$p.value > 0.05){
    null_hp<-'accepts'
    }else{
      null_hp<-'rejects'
  }
  ks_text<-paste('ks test',null_hp,'null hp. alpha=0.05',sep=' ')
  legend(8,0.2,legend=c('Fitted','Experimental',ks_text),col=c('black','red',''),lty=c(1,1,0))
}


#Author note: Notice that for sectorized weibull distributions we observe that the ks test accepts the null hypothesis,
#meaning that the fitted weibull can describe the observations at alpha=0.05. However this can also be since the
#sectors contain a fraction of the total observations, allowing the ks test to accept a higher superion than otherwise.

