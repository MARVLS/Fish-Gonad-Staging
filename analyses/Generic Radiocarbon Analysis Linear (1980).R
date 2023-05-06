rm(list=ls());  options(show.error.locations = TRUE)

library(readxl)

reference_series <- read_excel("C:/Users/derekc/Desktop/My Files/Research/Florida/Generic Radiocarbon Analysis/D14C Reference Series.xlsx")
radio <- read_excel("C:/Users/derekc/Desktop/My Files/Research/Florida/Generic Radiocarbon Analysis/Example Radiocarbon Results Linear.xlsx")

Snapper<-rbind(subset(reference_series,Type=="RS whole age-0"),subset(reference_series,Type=="RS edge"))
Coral<-subset(reference_series,Type=="Coral")


###############################################################################
#Linear Model Using >1980 data
###############################################################################
y<-as.numeric(unlist(subset(reference_series,Date>=1980)[,4]))
x<-as.numeric(unlist(subset(reference_series,Date>=1980)[,3]))
lm.out <- lm(y ~ x)
newx = seq(min(x-5),max(x+5),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="prediction",
                         level = 0.95)

###############################################################################
#Kastelle Analysis
###############################################################################
bias<-matrix(ncol=2,nrow=9)
bias[,1]<-c(-4,-3,-2,-1,0,+1,+2,+3,+4)

bias[5,2] <-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=radio$BY_DC)))^2)
bias[4,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC+1))))^2)
bias[3,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC+2))))^2)
bias[2,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC+3))))^2)
bias[1,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC+4))))^2)
bias[6,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC-1))))^2)
bias[7,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC-2))))^2)
bias[8,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC-3))))^2)
bias[9,2]<-sum((radio$D14C-predict(lm.out, newdata=data.frame(x=(radio$BY_DC-4))))^2)

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
#png(file="C:/Users/Derek C. Laptop/Desktop/My Files/Research/Florida/CRP Project/Radiocarbon/VS Radiocarbon/Plots/VS_Kastelle.png",width=8.5, height=11,units="in",res=300)
par(oma=c(3,3,0,0),mai = c(0.25,0.25,0.25,0.25),mfrow=c(3,3))
par(mfrow=c(3,3))

#empty plot 1
plot.new()
#no bias
{
  plot(x,y,xlim=c(1979,2020), ylim=c(20,160),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
  axis(1,at=c("",seq(1980,2020,by=5)),labels=c("",seq(1980,2020,by=5)),cex.axis=1.25)
  axis(1,at=seq(1979,2020,by=1),labels=F,tck=-0.01)
  axis(2,at=seq(20,160,by=10),labels=seq(20,160,by=10),las=1,cex.axis=1.25)
  axis(2,at=seq(20,160,by=2),labels=F,tck=-0.01)
  points(x=Snapper$Date,y=Snapper$D14,pch=22,bg="darkgrey")
  points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
  points(x=radio$`BY_DC`,y=radio$D14C,pch=24,bg="red")
  clip(1980,2017,20,160)
  abline(lm.out, col="black")
  lines(newx, conf_interval[,2], col="black", lty=2)
  lines(newx, conf_interval[,3], col="black", lty=2)
  text(1990,150,label="Null",cex=1.5)
  text(2005,120,label=paste("SSR=",round(bias[5,2],digits=0)),cex=1.25)
}
#empty plot 2
{
plot.new()
}
#plus 1 year
{
  plot(x,y,xlim=c(1979,2020), ylim=c(20,160),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
  axis(1,at=c("",seq(1980,2020,by=5)),labels=c("",seq(1980,2020,by=5)),cex.axis=1.25)
  axis(1,at=seq(1979,2020,by=1),labels=F,tck=-0.01)
  axis(2,at=seq(20,160,by=10),labels=seq(20,160,by=10),las=1,cex.axis=1.25)
  axis(2,at=seq(20,160,by=2),labels=F,tck=-0.01)
  points(x=Snapper$Date,y=Snapper$D14,pch=22,bg="darkgrey")
  points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
  points(x=radio$`BY_DC`-1,y=radio$D14C,pch=24,bg="red")
  clip(1980,2017,20,160)
  abline(lm.out, col="black")
  lines(newx, conf_interval[,2], col="black", lty=2)
  lines(newx, conf_interval[,3], col="black", lty=2)
  text(1990,150,label="+1",cex=1.5)
  text(2005,120,label=paste("SSR=",round(bias[6,2],digits=0)),cex=1.25)
}
#plus 2 year
{
  plot(x,y,xlim=c(1979,2020), ylim=c(20,160),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
  axis(1,at=c("",seq(1980,2020,by=5)),labels=c("",seq(1980,2020,by=5)),cex.axis=1.25)
  axis(1,at=seq(1979,2020,by=1),labels=F,tck=-0.01)
  axis(2,at=seq(20,160,by=10),labels=seq(20,160,by=10),las=1,cex.axis=1.25)
  axis(2,at=seq(20,160,by=2),labels=F,tck=-0.01)
  points(x=Snapper$Date,y=Snapper$D14,pch=22,bg="darkgrey")
  points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
  points(x=radio$`BY_DC`-2,y=radio$D14C,pch=24,bg="red")
  clip(1980,2017,20,160)
  abline(lm.out, col="black")
  lines(newx, conf_interval[,2], col="black", lty=2)
  lines(newx, conf_interval[,3], col="black", lty=2)
  text(1990,150,label="+2",cex=1.5)
  text(2005,120,label=paste("SSR=",round(bias[7,2],digits=0)),cex=1.25)
}
#plus 3 year
{
  plot(x,y,xlim=c(1979,2020), ylim=c(20,160),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
  axis(1,at=c("",seq(1980,2020,by=5)),labels=c("",seq(1980,2020,by=5)),cex.axis=1.25)
  axis(1,at=seq(1979,2020,by=1),labels=F,tck=-0.01)
  axis(2,at=seq(20,160,by=10),labels=seq(20,160,by=10),las=1,cex.axis=1.25)
  axis(2,at=seq(20,160,by=2),labels=F,tck=-0.01)
  points(x=Snapper$Date,y=Snapper$D14,pch=22,bg="darkgrey")
  points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
  points(x=radio$`BY_DC`-3,y=radio$D14C,pch=24,bg="red")
  clip(1980,2017,20,160)
  abline(lm.out, col="black")
  lines(newx, conf_interval[,2], col="black", lty=2)
  lines(newx, conf_interval[,3], col="black", lty=2)
  text(1990,150,label="+3",cex=1.5)
  text(2005,120,label=paste("SSR=",round(bias[8,2],digits=0)),cex=1.25)
}
#minus 1 year
{
  plot(x,y,xlim=c(1979,2020), ylim=c(20,160),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
  axis(1,at=c("",seq(1980,2020,by=5)),labels=c("",seq(1980,2020,by=5)),cex.axis=1.25)
  axis(1,at=seq(1979,2020,by=1),labels=F,tck=-0.01)
  axis(2,at=seq(20,160,by=10),labels=seq(20,160,by=10),las=1,cex.axis=1.25)
  axis(2,at=seq(20,160,by=2),labels=F,tck=-0.01)
  points(x=Snapper$Date,y=Snapper$D14,pch=22,bg="darkgrey")
  points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
  points(x=radio$`BY_DC`+1,y=radio$D14C,pch=24,bg="red")
  clip(1980,2017,20,160)
  abline(lm.out, col="black")
  lines(newx, conf_interval[,2], col="black", lty=2)
  lines(newx, conf_interval[,3], col="black", lty=2)
  text(1990,150,label="-1",cex=1.5)
  text(2005,120,label=paste("SSR=",round(bias[4,2],digits=0)),cex=1.25)
}
#minus 2 year
{
  plot(x,y,xlim=c(1979,2020), ylim=c(20,160),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
  axis(1,at=c("",seq(1980,2020,by=5)),labels=c("",seq(1980,2020,by=5)),cex.axis=1.25)
  axis(1,at=seq(1979,2020,by=1),labels=F,tck=-0.01)
  axis(2,at=seq(20,160,by=10),labels=seq(20,160,by=10),las=1,cex.axis=1.25)
  axis(2,at=seq(20,160,by=2),labels=F,tck=-0.01)
  points(x=Snapper$Date,y=Snapper$D14,pch=22,bg="darkgrey")
  points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
  points(x=radio$`BY_DC`+2,y=radio$D14C,pch=24,bg="red")
  clip(1980,2017,20,160)
  abline(lm.out, col="black")
  lines(newx, conf_interval[,2], col="black", lty=2)
  lines(newx, conf_interval[,3], col="black", lty=2)
  text(1990,150,label="-2",cex=1.5)
  text(2005,120,label=paste("SSR=",round(bias[3,2],digits=0)),cex=1.25)
}
#minus 3 year
{
  plot(x,y,xlim=c(1979,2020), ylim=c(20,160),col="white",frame.plot = F, axes=F,yaxs="i",xaxs="i")
  axis(1,at=c("",seq(1980,2020,by=5)),labels=c("",seq(1980,2020,by=5)),cex.axis=1.25)
  axis(1,at=seq(1979,2020,by=1),labels=F,tck=-0.01)
  axis(2,at=seq(20,160,by=10),labels=seq(20,160,by=10),las=1,cex.axis=1.25)
  axis(2,at=seq(20,160,by=2),labels=F,tck=-0.01)
  points(x=Snapper$Date,y=Snapper$D14,pch=22,bg="darkgrey")
  points(x=Coral$Date,y=Coral$D14,pch=21,bg="darkgrey")
  points(x=radio$`BY_DC`+3,y=radio$D14C,pch=24,bg="red")
  clip(1980,2017,20,160)
  abline(lm.out, col="black")
  lines(newx, conf_interval[,2], col="black", lty=2)
  lines(newx, conf_interval[,3], col="black", lty=2)
  text(1990,150,label="-3",cex=1.5)
  text(2005,120,label=paste("SSR=",round(bias[2,2],digits=0)),cex=1.25)
}
mtext(text="Year of formation",side=1,line=1,outer=TRUE,cex=1.15)
mtext(text=expression(paste(Delta^{14}, "C \u2030")),side=2,line=.85,outer=TRUE,cex=1.15)
}
#dev.off(which = dev.cur())
#plot(lm.out)