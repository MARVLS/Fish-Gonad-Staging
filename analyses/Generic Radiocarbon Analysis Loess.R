rm(list=ls());  options(show.error.locations = TRUE)

library(readxl)

reference_series <- read_excel("C:/Users/derekc/Desktop/My Files/Research/Florida/Generic Radiocarbon Analysis/D14C Reference Series.xlsx")
radio <- read_excel("C:/Users/derekc/Desktop/My Files/Research/Florida/Generic Radiocarbon Analysis/Example Radiocarbon Results.xlsx")

Snapper<-rbind(subset(reference_series,Type=="RS whole age-0"),subset(reference_series,Type=="RS edge"))
Coral<-subset(reference_series,Type=="Coral")

###############################################################################
#Loess Regression Full D14C Reference Series
###############################################################################
lowreg<-loess(D14~Date,data=reference_series,degree=2,span=0.2)

{
dev.new(width=12, height=8, unit="in",noRStudioGD = TRUE)
#png(file="C:/Users/Derek C. Laptop/Desktop/My Files/Research/Florida/CRP Project/Plots/Radiocarbon_1.png",width=7, height=7,units="in",res=300)
par(mar = c(5, 5.5, 3, 3))
par(mfrow=c(1,1))
plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
     xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i",cex.lab=2)
axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10),cex.axis=1.25)
axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.01)
axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50),cex.axis=1.25)
axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.01)
legend(1990,-20,legend = c("regional reference series",
                           "eye lens core"),
       pt.bg=c("darkgrey","red"),pch=c(21,22),cex=1.5)
j <- order(reference_series$Date)
lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
points(x=Snapper$Date,y=Snapper$D14,pch=21,bg="darkgrey",cex=1.5)
points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey",cex=1.5)
points(x=radio$`BY_DC`,y=radio$D14C,pch=22,bg="red",cex=1.5)
}

###############################################################################
#Kastelle Analysis
###############################################################################
expected<-predict(lowreg, newdata = radio$BY_DC)
observed<-radio$D14C

bias<-matrix(ncol=2,nrow=9)
bias[,1]<-c(-4,-3,-2,-1,0,+1,+2,+3,+4)

bias[5,2] <-sum((observed-predict(lowreg, newdata = radio$BY_DC))^2)
bias[4,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC+1))^2)
bias[3,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC+2))^2)
bias[2,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC+3))^2)
bias[1,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC+4))^2)
bias[6,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC-1))^2)
bias[7,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC-2))^2)
bias[8,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC-3))^2)
bias[9,2]<-sum((observed-predict(lowreg, newdata = radio$BY_DC-4))^2)
bias

{
  dev.new(width=4, height=2, unit="in",noRStudioGD = TRUE)
  #png(file="C:/Users/Derek C. Laptop/Desktop/My Files/Research/Florida/CRP Project/Radiocarbon/VS Radiocarbon/Plots/VS_Kastelle_legend.png",width=4, height=2,units="in",res=300)
  par(oma=c(3,3,0,0),mai = c(0.25,0.25,0.25,0.25))
  par(mfrow=c(1,1))
  plot.new()
  legend(0.5,0.7,legend = c("Gulf of Mexico & Caribbean coral","known-age red snapper otoliths","vermilion snapper eye lens core"),
         pch=c(21,22,24),pt.bg=c("darkgrey","darkgrey","red"),cex=0.5)
}

{
  dev.new(width=8.5, height=11, unit="in",noRStudioGD = TRUE)
  #png(file="C:/Users/Derek C. Laptop/Desktop/My Files/Research/Florida/CRP Project/Plots/Kastelle_plot.png",width=8.5, height=11,units="in",res=300)
  par(oma=c(3,3,0,0),mai = c(0.25,0.25,0.25,0.25),mfrow=c(3,3))
  par(mfrow=c(3,3))
  
  #empty plot 1
  plot.new()
  #Null
  {
    plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
         xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
    axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10))
    axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.015)
    axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50))
    axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.015)
    j <- order(reference_series$Date)
    lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
    #lines(newx, conf_interval[,2], col="black", lty=2)
    #lines(newx, conf_interval[,3], col="black", lty=2)
    points(x=Snapper$Date,y=Snapper$D14,pch=24,bg="darkgrey")
    points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
    points(x=radio$`BY_DC`,y=radio$D14C,pch=22,bg="red")
    text(1945,150,label="Null",cex=1.5,adj=0)
    text(1975,0,label=paste("SSR=",round(bias[5,2],digits=0)),cex=1,adj=0)
  }
  #empty plot 2
  plot.new()
  #Plus 1
  {
    plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
         xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
    axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10))
    axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.015)
    axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50))
    axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.015)
    j <- order(reference_series$Date)
    lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
    #lines(newx, conf_interval[,2], col="black", lty=2)
    #lines(newx, conf_interval[,3], col="black", lty=2)
    points(x=Snapper$Date,y=Snapper$D14,pch=24,bg="darkgrey")
    points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
    points(x=radio$`BY_DC`-1,y=radio$D14C,pch=22,bg="red")
    text(1945,150,label="+1",cex=1.5,adj=0)
    text(1975,0,label=paste("SSR=",round(bias[6,2],digits=0)),cex=1,adj=0)
  }
  #Plus 2
  {
    plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
         xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
    axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10))
    axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.015)
    axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50))
    axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.015)
    j <- order(reference_series$Date)
    lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
    #lines(newx, conf_interval[,2], col="black", lty=2)
    #lines(newx, conf_interval[,3], col="black", lty=2)
    points(x=Snapper$Date,y=Snapper$D14,pch=24,bg="darkgrey")
    points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
    points(x=radio$`BY_DC`-2,y=radio$D14C,pch=22,bg="red")
    text(1945,150,label="+2",cex=1.5,adj=0)
    text(1975,0,label=paste("SSR=",round(bias[7,2],digits=0)),cex=1,adj=0)
  }
  #Plus 3
  {
    plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
         xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
    axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10))
    axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.015)
    axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50))
    axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.015)
    j <- order(reference_series$Date)
    lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
    #lines(newx, conf_interval[,2], col="black", lty=2)
    #lines(newx, conf_interval[,3], col="black", lty=2)
    points(x=Snapper$Date,y=Snapper$D14,pch=24,bg="darkgrey")
    points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
    points(x=radio$`BY_DC`-3,y=radio$D14C,pch=22,bg="red")
    text(1945,150,label="+3",cex=1.5,adj=0)
    text(1975,0,label=paste("SSR=",round(bias[8,2],digits=0)),cex=1,adj=0)
  }
  #Minus 1
  {
    plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
         xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
    axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10))
    axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.015)
    axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50))
    axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.015)
    j <- order(reference_series$Date)
    lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
    #lines(newx, conf_interval[,2], col="black", lty=2)
    #lines(newx, conf_interval[,3], col="black", lty=2)
    points(x=Snapper$Date,y=Snapper$D14,pch=24,bg="darkgrey")
    points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
    points(x=radio$`BY_DC`+1,y=radio$D14C,pch=22,bg="red")
    text(1945,150,label="-1",cex=1.5,adj=0)
    text(1975,0,label=paste("SSR=",round(bias[4,2],digits=0)),cex=1,adj=0)
  }
  #Minus 2
  {
    plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
         xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
    axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10))
    axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.015)
    axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50))
    axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.015)
    j <- order(reference_series$Date)
    lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
    #lines(newx, conf_interval[,2], col="black", lty=2)
    #lines(newx, conf_interval[,3], col="black", lty=2)
    points(x=Snapper$Date,y=Snapper$D14,pch=24,bg="darkgrey")
    points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
    points(x=radio$`BY_DC`+2,y=radio$D14C,pch=22,bg="red")
    text(1945,150,label="-2",cex=1.5,adj=0)
    text(1975,0,label=paste("SSR=",round(bias[3,2],digits=0)),cex=1,adj=0)
  }
  #Minus 3
  {
    plot(D14~Date,data=reference_series,xlab="Year of Formation",ylab=expression(paste(Delta^{14}, "C (\u2030)")),
         xlim=c(1940,2017), ylim=c(-100,200),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
    axis(1,at=seq(1940,2017,by=10),labels=seq(1940,2017,by=10))
    axis(1,at=seq(1940,2017,by=1),labels=F,tck=-0.015)
    axis(2,at=seq(-100,200,by=50),labels=seq(-100,200,by=50))
    axis(2,at=seq(-100,200,by=10),labels=F,tck=-0.015)
    j <- order(reference_series$Date)
    lines(reference_series$Date[j],lowreg$fitted[j],col="black",lwd=2)
    #lines(newx, conf_interval[,2], col="black", lty=2)
    #lines(newx, conf_interval[,3], col="black", lty=2)
    points(x=Snapper$Date,y=Snapper$D14,pch=24,bg="darkgrey")
    points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
    points(x=radio$`BY_DC`+3,y=radio$D14C,pch=22,bg="red")
    text(1945,150,label="-3",cex=1.5,adj=0)
    text(1975,0,label=paste("SSR=",round(bias[2,2],digits=0)),cex=1,adj=0)
  }
  mtext(text="Year of formation",side=1,line=1,outer=TRUE,cex=1.15)
  mtext(text=expression(paste(Delta^{14}, "C \u2030")),side=2,line=.85,outer=TRUE,cex=1.15)
}
#dev.off(which = dev.cur())