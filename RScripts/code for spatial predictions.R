library(RColorBrewer)
library(classInt)
library(rgdal)

# data

pred.symp<-read.table("preds_R2_S1.txt",header=T)
pred<-pred.symp[,1]

# map

map.shp <- readOGR(dsn="C:/Users/lucp2188/Documents/COVID19",layer="Belgium_NIS (1)")

#Plot

nclr <- 8       #### number of colors to be used
plotvar <-  pred #### variable to be plotted
plotclr <- rev(brewer.pal(nclr,"RdYlGn"))         #### define colors
breaks<-c(round(min(pred),4)-0.0001,round(as.numeric(quantile(pred,probs = seq(0, 1, 0.125))),4)[2:8],
          round(max(pred),4)+0.0001)
class <- classIntervals(plotvar, nclr, style="fixed",fixedBreaks=breaks)    ### define categories
colcode <- findColours(class, plotclr)
plot(map.shp, col=colcode)
legend(3500000, 2639000,legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=1, bty="n")
title("Predicted probability of having at least 1 COVID-19 symptom\n(mean, CAR convolution model, corrected for age and gender)")
