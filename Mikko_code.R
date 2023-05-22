
################################################################
#
# R script for creating the Figures included in the article 
# "Advances in Development Reverse Fertility Decline
# by Mikko Myrsylä, Hans-Peter Kohler, Francesco C. Billari
# Nature, 2009
#
# This program, June 2009
#
# Use R (C) software (tested on version 2.8.1)
#
###############################################################/


### delete existing objects and newly define functions
rm(list=ls())
source("hdifert-functions.r")


### read data from data subdirectory (data file is "HDITFR_June_6_2009.csv")

tmp.data <- as.matrix(read.csv("Data/HDITFR_June_6_2009.csv",
                               row.names=1))
cnames <-  dimnames(tmp.data)[[1]]
years <- 1975:2005
hdi.data <- array(NA,
                  dim=c(dim(tmp.data)[1],31,4),
                  dimnames=list(dimnames(tmp.data)[[1]],
                                paste(years),c("HDI","TFR","TFR.STD","HDI.STD"))
)
hdi.data[,,1] <- tmp.data[,1:31]
hdi.data[,,2] <- tmp.data[,32:62]
adjTFR.data <- tmp.data[,93]

###############################################################/
###
### Cross-sectional plots
graphics.off()

## Main graph showing 1975 and 2005 cross-sectional HDI-TFR relationship:

transf.hdi <- function(x) -log(1-x)
pdf("Figures/figure-1.pdf",width=3.5,height=3.5)
par(cex=.55,cex.lab=1,font.lab=2,mar=c(3, 3, .05, .05),mgp=c(2,.8,0))
hdi.graph(hdi.data,1975,pcol=4,lcol=4,pch=15,lwd=2.5,
          do.cbox=T,lowess.f=.3,print.countries=T)
text(transf.hdi(.965),transf.tfr(7),"1975",cex=1.2,col=4,adj=1,font=2)
points(transf.hdi(.945),transf.tfr(7),col=4,pch=15,lwd=2.5)
hdi.graph(hdi.data,2005,newplot=F,pcol=2,lcol=2,pch=17,lwd=2.5,
          lowess.f=.25,print.countries=T)
text(transf.hdi(.965),transf.tfr(6),"2005",cex=1.2,col=2,adj=1,font=2)
points(transf.hdi(.945),transf.tfr(6),col=2,pch=17,lwd=2.5)
dev.off()

## Figure S.3 in Supplemental Information
## same as above, but include adjusted TFR
adj.hdi.data <- hdi.data
adj.hdi.data[,"2005","TFR"] <- adjTFR.data
pdf("Figures/figure-s2.pdf",width=3.5,height=3.5)
par(cex=.55,cex.lab=1,font.lab=2,mar=c(3, 3, .05, .05),mgp=c(2,.8,0))
hdi.graph(hdi.data,1975,pcol=4,lcol=4,pch=15,lwd=2.5,do.cbox=T,
          lowess.f=.3,print.countries=T)
text(transf.hdi(.965),transf.tfr(7),"1975",cex=1.2,col=4,adj=1,font=2)
points(transf.hdi(.945),transf.tfr(7),col=4,pch=15,lwd=2.5)
hdi.graph(hdi.data,2005,newplot=F,pcol=2,lcol=2,pch=17,lwd=2.5,
          lowess.f=.25,print.countries=T)
text(transf.hdi(.965),transf.tfr(6),"2005",cex=1.2,col=2,adj=1,font=2)
points(transf.hdi(.945),transf.tfr(6),col=2,pch=17,lwd=2.5)
hdi.graph(adj.hdi.data,2005,newplot=F,pcol=1,lcol=1,pch=19,lwd=2.5,
          lowess.f=.5,print.countries=T)
text(transf.hdi(.965),transf.tfr(5.1),"2005",cex=1.2,col=1,adj=1,font=2)
text(transf.hdi(.965),transf.tfr(4.5),"with adj. TFR",cex=1.2,col=1,adj=1,
     font=2)
points(transf.hdi(.945),transf.tfr(5.1),col=1,pch=19,lwd=2.5)
dev.off()

###############################################################/
###
### Longitudinal trend with reversal around .85
graphics.off()

hdi.figinclude <- .9
plot.data <- hdi.data[hdi.data[,"2005","HDI"] >= hdi.figinclude &
                        apply(hdi.data[,,1:2],1,function(x) sum(is.na(x))) == 0,,]
plot.n.countries <- dim(plot.data)[1]
plot.countries <- dimnames(plot.data)[[1]]
plot.refinfo <- matrix(NA,plot.n.countries,3,dimnames=list(plot.countries,
                                                           c("year","HDI","TFR")))
for (i in 1:plot.n.countries){
  fert.ref <- tfr.ref(plot.data[i,,1],plot.data[i,,2],c.window=c(.85,.9))
  plot.refinfo[i,"year"] <- fert.ref$year
  plot.refinfo[i,"HDI"] <- fert.ref$hdi
  plot.refinfo[i,"TFR"] <- fert.ref$tfr
  plot.data[i,,3] <- (plot.data[i,,2] - fert.ref$tfr)
  plot.data[i,,4] <- plot.data[i,,1]-fert.ref$hdi
  print(plot.countries[i])
  print(plot.data[i,,])
  print(fert.ref)
}
jj <- order(-(plot.data[,31,3]/plot.data[,31,4]))
plot.data <- plot.data[jj,,]
plot.refinfo <- plot.refinfo[jj,]
plot.countries <- plot.countries[jj]


plot.full <- c("Norway","Japan","NL","USA")
plot.points <- plot.countries[-match(plot.full,plot.countries)]
plot.data.full <- plot.data[match(plot.full,plot.countries),,]
plot.ref.full <- plot.refinfo[match(plot.full,plot.countries),]
plot.data.points <- plot.data[-match(plot.full,plot.countries),,]
plot.ref.points <- plot.refinfo[-match(plot.full,plot.countries),]
plot.cols <- c(1,3,4,2,6)
plot.lty <- as.vector(matrix(2:5,4,4,byrow=T))

transf.hdi <- function(x) x
pdf("Figures/figure-2.pdf",width=3.5,height=3.5)
par(cex=.55,cex.lab=1,font.lab=2,mar=c(3, 3, .05, .05),mgp=c(2,.8,0))
matplot(t(plot.data[,,4]),t(plot.data[,,3]),
        xlim=c(-.1,.1),
        ylim=c(-.5,1),
        type="n",xlab="change in HDI compared to reference year",
        ylab="change in TFR compared to reference year")
abline(h=0,v=0,lty=plot.lty)
text(-.075,-.5,"1975",adj=1)
text( .075,-.5,"2005",adj=0)
text(-.001,-.5,"ref.  year",adj=.37)
arrows(-.073,-.5,-.014,-.5,length=.06)
arrows( .019,-.5, .073,-.5,length=.06)
matlines(rbind(plot.data.points[,1,4],
               rep(0,length(plot.ref.points[,"HDI"])),
               plot.data.points[,31,4]),
         rbind(plot.data.points[,1,3],
               0,
               plot.data.points[,31,3]),
         lty = 2:5,lwd=1.3,col=plot.cols,
         type="l")
matpoints(rbind(plot.data.points[,1,4],
                plot.data.points[,31,4]),
          rbind(plot.data.points[,1,3],
                plot.data.points[,31,3]),
          col=plot.cols,
          pch=2:9,bg=plot.cols,cex=.8)    
symbols(0,0,circle=.005,add=T,inches=F,fg=grey(.95),bg=grey(.95))


t.x.offset <- rep(-.0025,length(plot.points))
t.y.offset <- rep(.005,length(plot.points))
t.x.offset[9] <- .006
t.x.offset[11] <- .007
t.x.offset[13] <- .0025
t.y.offset[13] <- .025
text(plot.data.points[,"1975",4] + t.x.offset,
     plot.data.points[,"1975",3] + t.y.offset,
     1:length(plot.points),cex=.8,col=plot.cols,adj=1)

t.x.offset <- rep(.0025,length(plot.points))
t.y.offset <- rep(.005,length(plot.points))
t.x.offset[2] <- -.0025
t.y.offset[2] <- .03
t.y.offset[3] <- .015
t.x.offset[4] <- .0015
t.y.offset[4] <- .025
t.y.offset[6] <- -.005
t.y.offset[14] <- 0
t.y.offset[16] <- -.007
t.y.offset[17] <- -.005
t.y.offset[17] <- -.005
t.y.offset[18] <- -.005
t.y.offset[19] <- -.005
t.x.offset[20] <- 0
t.y.offset[20] <- -.03
text(plot.data.points[,"2005",4] + t.x.offset,
     plot.data.points[,"2005",3] + t.y.offset,
     1:length(plot.points),cex=.8,col=plot.cols,adj=0)

matlines(t(plot.data.full[,,4]),t(plot.data.full[,,3]),
         col=plot.cols,
         lty=1,lwd=1.8)
points(plot.data.full[,"1975",4],
       plot.data.full[,"1975",3],
       col=plot.cols,bg=plot.cols,pch=22:25,cex=.8)
points(plot.data.full[,"2005",4],
       plot.data.full[,"2005",3],
       col=plot.cols,bg=plot.cols,pch=22:25,cex=.8)

text(plot.data.full[,"2005",4] + c(-.0025,.0025,-.0025,0),
     plot.data.full[,"2005",3] + c(.025,-.005,-.03,.025),
     plot.full,adj=0,cex=.8,col=plot.cols)
dev.off()


###############################################################/
###
### Figure 3: Results of regression models

modelresults <- array(NA,dim=c(2,4,2),
                      dimnames=list(c("pre","post"),
                                    c("Model 1","Model 2","Model 3","Model 4"),
                                    c("Coef","Std. Error")))
modelresults["pre","Model 1",] <- c(-1.585792768,0.7798566346)
modelresults["post","Model 1",] <- c(4.0731134173,0.9310119761)

modelresults["pre","Model 2",] <- c(-1.068881422,0.7914105362)
modelresults["post","Model 2",] <- c(4.1716930541,0.9186609097)

modelresults["pre","Model 3",] <- c(-1.040272684,1.3661912916)
modelresults["post","Model 3",] <- c(2.8529402591,1.2709392004)

modelresults["pre","Model 4",] <- c(-1.570033906,1.072362211)
modelresults["post","Model 4",] <- c(3.058872834,0.9491880747)


pdf("Figures/figure-2.pdf",width=3.5,height=2.5)
par(cex=.55,cex.lab=1,font.lab=2,mar=c(2, 2, .05, .05),mgp=c(2,.8,0))
xloc <- barplot(modelresults[,,"Coef"],beside=T,ylim=c(-4,7),
                col=c(grey(.75),gray(.25)),
                font.lab=2,font.axis=2,yaxt="n")
axis(2)
legend("topright",
       expression("at HDI" < .86,"at HDI" >= 0.86),
       fill=c(grey(.75),gray(.25)),bty="n",cex=.8,pt.cex=5)

abline(h=0,lty=1)
arrows(as.vector(xloc),
       as.vector(modelresults[,,"Coef"]-1.96*modelresults[,,"Std. Error"]),
       as.vector(xloc),
       as.vector(modelresults[,,"Coef"]+1.96*modelresults[,,"Std. Error"]),
       length=0.04, angle=90, code=3)
dev.off()


###############################################################/
###
### Additional calculations reported in text or supplemental
### information

### statistics for body of paper
data.1975.ok <- !apply(hdi.data[,"1975",c("HDI","TFR")],
                       1,function(x) any(is.na(x)))
data.2005.ok <- !apply(hdi.data[,"2005",c("HDI","TFR")],
                       1,function(x) any(is.na(x)))

### countries with moderate and high HDI
hdi.9to.92 <- tfr.by.hdi(hdi.data,2005,.9,.92)
hdi.above.95 <- tfr.by.hdi(hdi.data,2005,.95,1)
hdi.above.9 <- tfr.by.hdi(hdi.data,2005,.9,1)

### Number of top HDI countries by TFR range
hdi.above.9
nrow(hdi.above.9$countries)
sum(hdi.above.9$countries[,"TFR"] < 1.3)
sum(hdi.above.9$countrie[,"TFR"]  >= 1.3 & hdi.above.9$countrie[,"TFR"] < 1.5)
sum(hdi.above.9$countrie[,"TFR"]  >= 1.5 & hdi.above.9$countrie[,"TFR"] < 2.1)
sum(hdi.above.9$countrie[,"TFR"]  >= 2.1)



### Figure 1: number of countries with HDI below .85 and
### above .9, with 1975 and 2005 cross-sectional HDI-TFR
## correlations, and list of countries with HDI > .9.
sum(data.1975.ok)
range(hdi.data[data.1975.ok,"1975","HDI"])
range(hdi.data[data.1975.ok,"1975","TFR"])
cor.test(tfr.by.hdi(hdi.data,1975,0,1)$countries[,1],
         tfr.by.hdi(hdi.data,1975,0,1)$countries[,2],meth="spearman")
sum(data.2005.ok)
range(hdi.data[data.2005.ok,"2005","HDI"])
range(hdi.data[data.2005.ok,"2005","TFR"])
cor.test(tfr.by.hdi(hdi.data,2005,0,.85)$countries[,1],
         tfr.by.hdi(hdi.data,2005,0,.85)$countries[,2],meth="spearman")
cor.test(tfr.by.hdi(hdi.data,2005,.9,1)$countries[,1],
         tfr.by.hdi(hdi.data,2005,.9,1)$countries[,2],meth="spearman")
paste(dimnames(hdi.above.9$countries)[[1]]," (",
      round(hdi.above.9$countries[,"HDI"],3),")",sep="")


### TFR of countries with 0.9 < HDI <= 0.92 and with 0.95 < HDI <= 1
hdi.9to.92
hdi.above.95

### Halving time
log(2)/(log(.4886*hdi.9to.92$mean.tfr)/31)


### Figure 2:
### Countries for longitudional analyses
plot.countries
plot.n.countries

### Countries by quadrant
in.topright.full <- plot.data.full[,"2005",3]>=0
in.topright.points<- plot.data.points[,"2005",3]>=0
in.bottomright.full <- plot.data.full[,"2005",3]< 0
in.bottomright.points<- plot.data.points[,"2005",3]< 0

### Countries ending in top right quadrant
print(c(paste(plot.full[in.topright.full],
              ", ", sep=""),
        paste("(",(1:length(plot.points))[in.topright.points],") ",
              plot.points[in.topright.points],
              ", ", sep="")
))

### Countries ending in bottom right quadrant 
print(c(paste(plot.full[in.bottomright.full],
              ", ", sep=""),
        paste("(",(1:length(plot.points))[in.bottomright.points],") ",
              plot.points[in.bottomright.points],
              ", ", sep="")
))

### change per HDI  unit since ref year
dTFR.dHDI <- (plot.data[,"2005","TFR"] - plot.refinfo[,"TFR"])/
  (plot.data[,"2005","HDI"] - plot.refinfo[,"HDI"])

### Change in TFR per .05 change in HDI
dTFR.dHDI * .05
mean(dTFR.dHDI[dTFR.dHDI >=0]) * .05
mean(dTFR.dHDI) * .05


## correlations with adjusted TFR
data.2005adj.ok <- !apply(adj.hdi.data[,"2005",c("HDI","TFR")],
                          1,function(x) any(is.na(x)))
sum(data.2005adj.ok)
range(adj.hdi.data[data.2005adj.ok,"2005","HDI"])
range(adj.hdi.data[data.2005adj.ok,"2005","TFR"])
cor.test(tfr.by.hdi(adj.hdi.data,2005,.9,1)$countries[,1],
         tfr.by.hdi(adj.hdi.data,2005,.9,1)$countries[,2],meth="spearman")





