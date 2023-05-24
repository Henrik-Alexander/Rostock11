### Time-stamp: <2009-06-11 15:30:31 hpkohler hdifert-functions.r>

first.above <- function(hdi,crit){
  ii <- seq(1,by=1,l=length(hdi))
  min(ii[hdi >= crit])
}

last.below <- function(hdi,crit){
  ii <- seq(1,by=1,l=length(hdi))
  max(ii[hdi <= crit])
}

tfr.ref <- function(hdi,tfr,year=1975:2005,c.window=c(.85,.90))
{
  ii <- seq(1,by=1,l=length(hdi))
  in.range <- ii >= first.above(hdi,c.window[1]) &
    ii <= last.below(hdi,c.window[2])
  reftfr <- min(tfr[in.range])
  refyear <- min(year[in.range][tfr[in.range] == reftfr])
  list(tfr = reftfr,
       hdi = hdi[year==refyear],
       year = refyear
  )
}

tfr.by.hdi <- function(all.data,year,from=.9,to=1)
{
  data.ok <- !apply(all.data[,paste(year),c("HDI","TFR")],
                    1,function(x) any(is.na(x)))
  c.data <- all.data[all.data[,paste(year),"HDI"] > from &
                       all.data[,"2005","HDI"] <= to & data.ok,paste(year),
                     c("HDI","TFR")]
  c.data <- c.data[order(-c.data[,"HDI"]),]
  list(hdi.limits = paste(from,"< HDI <=",to),
       countries = c.data,
       mean.tfr = mean(c.data[,"TFR"]),
       mean.hdi = mean(c.data[,"HDI"]),
       from = from,
       to = to)
}




transf.tfr <- function(x, mu=31,prop.fem=.4886) log(prop.fem * x)/mu
transf.hdi <- function(x) -log(1-x)

hdi.graph <-
  function(all.data,year,transf=T,newplot=T,
           pcol="darkred",lcol="darkblue",pch=21,lwd=2,cex=1,
           do.xaxis=T,do.yaxis = T,do.cbox=F,
           add.smooth=T,
           lowess.f=.275,
           xlab="Human Development Index (HDI)",
           ylab="Total Fertility Rate (TFR)",
           hdi.range = range(c(all.data[,"1975","HDI"],
                               all.data[,"2005","HDI"]),na.rm=T),
           tfr.range = range(c(all.data[,"1975","TFR"],
                               all.data[,"2005","TFR"]),na.rm=T),
           plot.hdi.ticks = c(.1,.3,.5,.7,.8,.85,.9,.95,.98),
           plot.tfr.ticks = c(1,2,3,4,5,6,7,8),
           plot.hdi.ticks2 = c(.3,.6,.8,.9,.95,.98),
           plot.tfr.ticks2 = c(1.2,1.5,2,3,4,6,8),
           print.countries=F)
  {
    data.ok <- !apply(all.data[,paste(year),c("HDI","TFR")],
                      1,function(x) any(is.na(x)))
    hdi <- all.data[data.ok,paste(year),"HDI"]
    tfr <- all.data[data.ok,paste(year),"TFR"]
    if(transf)
    {
      tr.hdi <- transf.hdi(hdi)
      tr.tfr <- transf.tfr(tfr)
      if(newplot){
        plot(tr.hdi,tr.tfr,type="n",
             xaxt="n",yaxt="n",lwd=lwd,
             xlim=transf.hdi(hdi.range),
             ylim=transf.tfr(tfr.range),
             xlab=xlab,
             ylab=ylab)
      }
      if(do.cbox)  rect(transf.hdi(.85),transf.tfr(tfr.range[1]),
                        transf.hdi(.90),transf.tfr(tfr.range[2]),
                        border=NA,col=grey(.9))
      points(tr.hdi,tr.tfr,lwd=1,col=pcol,pch=pch,cex=cex)
      if(do.xaxis){
        axis(1,at=transf.hdi(plot.hdi.ticks2),labels=paste(plot.hdi.ticks2))
      }
      if(do.yaxis){
        axis(2,at=transf.tfr(plot.tfr.ticks2),labels=paste(plot.tfr.ticks2))
      }
      if(add.smooth){
        lines(lowess(tr.hdi,tr.tfr,f=lowess.f),col=lcol,lwd=lwd)
      }	
    }else{
      if(newplot){
        plot(hdi,tfr,type="n",
             xaxt="n",yaxt="n",lwd=lwd,
             xlim=hdi.range,
             ylim=tfr.range,
             xlab=xlab,
             ylab=ylab)
      }
      points(hdi,tfr,lwd=1,col=pcol,pch=pch,cex=cex)
      if(do.xaxis){
        axis(1,at=plot.hdi.ticks,labels=paste(plot.hdi.ticks))
      }
      if(do.yaxis){
        axis(2,at=plot.tfr.ticks,labels=paste(plot.tfr.ticks))
      }
      if(add.smooth){
        lines(smooth.spline(tr.hdi,tr.tfr),col=lcol,lwd=3)
      }
    }
    print(paste("Number of countries (",paste(year),"): ",sum(data.ok),sep=""))
    if(print.countries) print(dimnames(all.data)[[1]][data.ok])
  }

