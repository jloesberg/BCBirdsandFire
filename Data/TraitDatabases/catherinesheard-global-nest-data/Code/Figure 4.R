library(RColorBrewer)

pdf("Figure2.pdf",height=8,width=15)
par(mar = c(2.5, 4.5, 2, 2))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

##############
## Panel A
##############

x<-read.csv("Dataset-S2.csv") #read in data
x$PhyloName<-gsub(" ", "_",x$PhyloName)

#prepare the data
if(sum(is.na(x$PhyloName)>0)){x<-x[-which(is.na(x$PhyloName)),]}
if(sum(is.na(x$Body.mass..log.)>0)){x<-x[-which(is.na(x$Body.mass..log.)),]}
if(sum(is.na(x$Str.None)>0)){x<-x[-which(is.na(x$Str.None)),]}
if(sum(x$Str.None=="u")>0){x<-x[-which(x$Str.None=="u"),]}
if(sum(is.na(x$Str.Platform)>0)){x<-x[-which(is.na(x$Str.Platform)),]}
if(sum(x$Str.Platform=="u")>0){x<-x[-which(x$Str.Platform=="u"),]}
if(sum(is.na(x$Str.Cup)>0)){x<-x[-which(is.na(x$Str.Cup)),]}
if(sum(x$Str.Cup=="u")>0){x<-x[-which(x$Str.Cup=="u"),]}
if(sum(is.na(x$Str.Scrape)>0)){x<-x[-which(is.na(x$Str.Scrape)),]}
if(sum(x$Str.Scrape=="u")>0){x<-x[-which(x$Str.Scrape=="u"),]}
if(sum(is.na(x$Str.Excavation)>0)){x<-x[-which(is.na(x$Str.Excavation)),]}
if(sum(x$Str.Excavation=="u")>0){x<-x[-which(x$Str.Excavation=="u"),]}
if(sum(is.na(x$Str.Dome)>0)){x<-x[-which(is.na(x$Str.Dome)),]}
if(sum(x$Str.Dome=="u")>0){x<-x[-which(x$Str.Dome=="u"),]}
if(sum(is.na(x$Str.DomeAndTube)>0)){x<-x[-which(is.na(x$Str.DomeAndTube)),]}
if(sum(x$Str.DomeAndTube=="u")>0){x<-x[-which(x$Str.DomeAndTube=="u"),]}


hwis<-x$Body.mass..log.[which(x$Str.Cup==1)]
labels<-rep("cup",length(x$Body.mass..log.[which(x$Str.Cup==1)]))

hwis<-c(hwis,x$Body.mass..log.[which(x$Str.None==1)])
labels<-c(labels,rep("none",length(x$Body.mass..log.[which(x$Str.None==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Str.Platform==1)])
labels<-c(labels,rep("platform",length(x$Body.mass..log.[which(x$Str.Platform==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Str.Scrape==1)])
labels<-c(labels,rep("scrape",length(x$Body.mass..log.[which(x$Str.Scrape==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Str.Excavation==1)])
labels<-c(labels,rep("ex.",length(x$Body.mass..log.[which(x$Str.Excavation==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Str.Dome==1)])
labels<-c(labels,rep("dome",length(x$Body.mass..log.[which(x$Str.Dome==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Str.DomeAndTube==1)])
labels<-c(labels,rep("dome-tube",length(x$Body.mass..log.[which(x$Str.DomeAndTube==1)])))


cols<-brewer.pal(7,"Pastel2")

plot(hwis~as.factor(labels),col=cols,ylab="Body Mass (logged)",xlab="Nest Structure",cex.lab=1.5,cex.axis=1.1)
text("A.",x=0.5,y=4.8,cex=2.5)
text("#",x=2.2,y=0.5,cex=2,col="blue")
text("#",x=4.2,y=0.5,cex=2,col="blue")
text("*",x=6.2,y=0.5,cex=3.5,col="red")
text("*",x=7.2,y=0.5,cex=3.5,col="red")


##############
## Panel B
##############

x<-read.csv("Dataset-S2.csv") #read in data
x$PhyloName<-gsub(" ", "_",x$PhyloName)

#prepare the data
if(sum(is.na(x$PhyloName)>0)){x<-x[-which(is.na(x$PhyloName)),]}
if(sum(is.na(x$Body.mass..log.)>0)){x<-x[-which(is.na(x$Body.mass..log.)),]}
if(sum(is.na(x$Loc.Artificial)>0)){x<-x[-which(is.na(x$Loc.Artificial)),]}
if(sum(x$Loc.Artificial=="u")>0){x<-x[-which(x$Loc.Artificial=="u"),]}
if(sum(is.na(x$Loc.Ground)>0)){x<-x[-which(is.na(x$Loc.Ground)),]}
if(sum(x$Loc.Ground=="u")>0){x<-x[-which(x$Loc.Ground=="u"),]}
if(sum(is.na(x$Loc.EarthHole)>0)){x<-x[-which(is.na(x$Loc.EarthHole)),]}
if(sum(x$Loc.EarthHole=="u")>0){x<-x[-which(x$Loc.EarthHole=="u"),]}
if(sum(is.na(x$Loc.Rocks)>0)){x<-x[-which(is.na(x$Loc.Rocks)),]}
if(sum(x$Loc.Rocks=="u")>0){x<-x[-which(x$Loc.Rocks=="u"),]}
if(sum(is.na(x$Loc.TreeHole)>0)){x<-x[-which(is.na(x$Loc.TreeHole)),]}
if(sum(x$Loc.TreeHole=="u")>0){x<-x[-which(x$Loc.TreeHole=="u"),]}
if(sum(is.na(x$Loc.Water)>0)){x<-x[-which(is.na(x$Loc.Water)),]}
if(sum(x$Loc.Water=="u")>0){x<-x[-which(x$Loc.Water=="u"),]}
if(sum(is.na(x$Loc.Veg)>0)){x<-x[-which(is.na(x$Loc.Veg)),]}
if(sum(x$Loc.Veg=="u")>0){x<-x[-which(x$Loc.Veg=="u"),]}


hwis<-x$Body.mass..log.[which(x$Loc.EarthHole==1)]
labels<-rep("earth hole",length(x$Body.mass..log.[which(x$Loc.EarthHole==1)]))

hwis<-c(hwis,x$Body.mass..log.[which(x$Loc.Artificial==1)])
labels<-c(labels,rep("artificial",length(x$Body.mass..log.[which(x$Loc.Artificial==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Loc.Ground==1)])
labels<-c(labels,rep("ground",length(x$Body.mass..log.[which(x$Loc.Ground==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Loc.Rocks==1)])
labels<-c(labels,rep("rock",length(x$Body.mass..log.[which(x$Loc.Rocks==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Loc.TreeHole==1)])
labels<-c(labels,rep("tree hole",length(x$Body.mass..log.[which(x$Loc.TreeHole==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Loc.Water==1)])
labels<-c(labels,rep("water",length(x$Body.mass..log.[which(x$Loc.Water==1)])))

hwis<-c(hwis,x$Body.mass..log.[which(x$Loc.Veg==1)])
labels<-c(labels,rep("vegetation",length(x$Body.mass..log.[which(x$Loc.Veg==1)])))

cols<-brewer.pal(7,"Set1")

plot(hwis~as.factor(labels),col=cols,ylab="Body Mass (logged)",xlab="Nest Location",cex.lab=1.5,cex.axis=1.1)
text("B.",x=0.5,y=4.8,cex=2.5)
text("*",x=1.2,y=0.5,cex=3.5,col="red")
text("*",x=4.2,y=0.5,cex=3.5,col="red")
text("#",x=6.2,y=0.5,cex=2,col="blue")

##############
## Panel C
##############

x<-read.csv("Dataset-S2.csv") #read in data
x$PhyloName<-gsub(" ", "_",x$PhyloName)

#prepare the data
if(sum(is.na(x$PhyloName)>0)){x<-x[-which(is.na(x$PhyloName)),]}
if(sum(is.na(x$HWI)>0)){x<-x[-which(is.na(x$HWI)),]}
if(sum(is.na(x$Str.None)>0)){x<-x[-which(is.na(x$Str.None)),]}
if(sum(x$Str.None=="u")>0){x<-x[-which(x$Str.None=="u"),]}
if(sum(is.na(x$Str.Platform)>0)){x<-x[-which(is.na(x$Str.Platform)),]}
if(sum(x$Str.Platform=="u")>0){x<-x[-which(x$Str.Platform=="u"),]}
if(sum(is.na(x$Str.Cup)>0)){x<-x[-which(is.na(x$Str.Cup)),]}
if(sum(x$Str.Cup=="u")>0){x<-x[-which(x$Str.Cup=="u"),]}
if(sum(is.na(x$Str.Scrape)>0)){x<-x[-which(is.na(x$Str.Scrape)),]}
if(sum(x$Str.Scrape=="u")>0){x<-x[-which(x$Str.Scrape=="u"),]}
if(sum(is.na(x$Str.Excavation)>0)){x<-x[-which(is.na(x$Str.Excavation)),]}
if(sum(x$Str.Excavation=="u")>0){x<-x[-which(x$Str.Excavation=="u"),]}
if(sum(is.na(x$Str.Dome)>0)){x<-x[-which(is.na(x$Str.Dome)),]}
if(sum(x$Str.Dome=="u")>0){x<-x[-which(x$Str.Dome=="u"),]}
if(sum(is.na(x$Str.DomeAndTube)>0)){x<-x[-which(is.na(x$Str.DomeAndTube)),]}
if(sum(x$Str.DomeAndTube=="u")>0){x<-x[-which(x$Str.DomeAndTube=="u"),]}


hwis<-x$HWI[which(x$Str.Cup==1)]
labels<-rep("cup",length(x$HWI[which(x$Str.Cup==1)]))

hwis<-c(hwis,x$HWI[which(x$Str.None==1)])
labels<-c(labels,rep("none",length(x$HWI[which(x$Str.None==1)])))

hwis<-c(hwis,x$HWI[which(x$Str.Platform==1)])
labels<-c(labels,rep("platform",length(x$HWI[which(x$Str.Platform==1)])))

hwis<-c(hwis,x$HWI[which(x$Str.Scrape==1)])
labels<-c(labels,rep("scrape",length(x$HWI[which(x$Str.Scrape==1)])))

hwis<-c(hwis,x$HWI[which(x$Str.Excavation==1)])
labels<-c(labels,rep("ex.",length(x$HWI[which(x$Str.Excavation==1)])))

hwis<-c(hwis,x$HWI[which(x$Str.Dome==1)])
labels<-c(labels,rep("dome",length(x$HWI[which(x$Str.Dome==1)])))

hwis<-c(hwis,x$HWI[which(x$Str.DomeAndTube==1)])
labels<-c(labels,rep("dome-tube",length(x$HWI[which(x$Str.DomeAndTube==1)])))


library(RColorBrewer)

cols<-brewer.pal(7,"Pastel2")

plot(hwis~as.factor(labels),col=cols,ylab="Hand-wing index",xlab="Nest Structure",cex.lab=1.5,cex.axis=1.1)
text("C.",x=0.5,y=70,cex=2.5)
text("*",x=1.2,y=8,cex=3.5,col="red")
text("#",x=2.2,y=8,cex=2,col="blue")
text("#",x=3.2,y=8,cex=2,col="blue")
text("*",x=5.2,y=8,cex=3.5,col="red")
text("*",x=7.2,y=8,cex=3.5,col="red")

##############
## Panel D
##############

x<-read.csv("Dataset-S2.csv") #read in data
x$PhyloName<-gsub(" ", "_",x$PhyloName)

#prepare the data
if(sum(is.na(x$PhyloName)>0)){x<-x[-which(is.na(x$PhyloName)),]}
if(sum(is.na(x$HWI)>0)){x<-x[-which(is.na(x$HWI)),]}
if(sum(is.na(x$Loc.Artificial)>0)){x<-x[-which(is.na(x$Loc.Artificial)),]}
if(sum(x$Loc.Artificial=="u")>0){x<-x[-which(x$Loc.Artificial=="u"),]}
if(sum(is.na(x$Loc.Ground)>0)){x<-x[-which(is.na(x$Loc.Ground)),]}
if(sum(x$Loc.Ground=="u")>0){x<-x[-which(x$Loc.Ground=="u"),]}
if(sum(is.na(x$Loc.EarthHole)>0)){x<-x[-which(is.na(x$Loc.EarthHole)),]}
if(sum(x$Loc.EarthHole=="u")>0){x<-x[-which(x$Loc.EarthHole=="u"),]}
if(sum(is.na(x$Loc.Rocks)>0)){x<-x[-which(is.na(x$Loc.Rocks)),]}
if(sum(x$Loc.Rocks=="u")>0){x<-x[-which(x$Loc.Rocks=="u"),]}
if(sum(is.na(x$Loc.TreeHole)>0)){x<-x[-which(is.na(x$Loc.TreeHole)),]}
if(sum(x$Loc.TreeHole=="u")>0){x<-x[-which(x$Loc.TreeHole=="u"),]}
if(sum(is.na(x$Loc.Water)>0)){x<-x[-which(is.na(x$Loc.Water)),]}
if(sum(x$Loc.Water=="u")>0){x<-x[-which(x$Loc.Water=="u"),]}
if(sum(is.na(x$Loc.Veg)>0)){x<-x[-which(is.na(x$Loc.Veg)),]}
if(sum(x$Loc.Veg=="u")>0){x<-x[-which(x$Loc.Veg=="u"),]}


hwis<-x$HWI[which(x$Loc.EarthHole==1)]
labels<-rep("earth hole",length(x$HWI[which(x$Loc.EarthHole==1)]))

hwis<-c(hwis,x$HWI[which(x$Loc.Artificial==1)])
labels<-c(labels,rep("artificial",length(x$HWI[which(x$Loc.Artificial==1)])))

hwis<-c(hwis,x$HWI[which(x$Loc.Ground==1)])
labels<-c(labels,rep("ground",length(x$HWI[which(x$Loc.Ground==1)])))

hwis<-c(hwis,x$HWI[which(x$Loc.Rocks==1)])
labels<-c(labels,rep("rock",length(x$HWI[which(x$Loc.Rocks==1)])))

hwis<-c(hwis,x$HWI[which(x$Loc.TreeHole==1)])
labels<-c(labels,rep("tree hole",length(x$HWI[which(x$Loc.TreeHole==1)])))

hwis<-c(hwis,x$HWI[which(x$Loc.Water==1)])
labels<-c(labels,rep("water",length(x$HWI[which(x$Loc.Water==1)])))

hwis<-c(hwis,x$HWI[which(x$Loc.Veg==1)])
labels<-c(labels,rep("vegetation",length(x$HWI[which(x$Loc.Veg==1)])))


library(RColorBrewer)

cols<-brewer.pal(7,"Set1")
plot(hwis~as.factor(labels),col=cols,ylab="Hand-wing index",xlab="Nest Location",cex.lab=1.5,cex.axis=1.1)
text("D.",x=0.5,y=70,cex=2.5)
text("*",x=1.2,y=8,cex=3.5,col="red")
text("*",x=2.2,y=8,cex=3.5,col="red")
text("*",x=4.2,y=8,cex=3.5,col="red")
text("#",x=6.2,y=8,cex=2,col="blue")

dev.off()
