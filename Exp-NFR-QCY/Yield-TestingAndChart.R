library(psych)
library(Lahman)
library(dplyr)
library(MASS)
library(effsize)
library(reshape2)
library(ggplot2)
library(Rmisc)
library(gridExtra)

### For calculating speed of elicitation, yield
### and printing the charts

inputFileName <- "yield"
outputFileName <- "YieldChart"
digPrec<-4 
outputFilesPath <- paste("output/")
pathToFile <- paste("input/",inputFileName,".csv", sep='')


data <-read.csv(pathToFile, sep=";", header=TRUE, encoding = "UTF-8")

getPlot <- function()
{
data$GroupType <- factor(data$GroupType, levels=c("single", "group"))
data$GroupType <- revalue(data$GroupType, c("group"= "Teams"))
data$GroupType <- revalue(data$GroupType, c("single"= "Individuals"))
data$Method <- factor(data$Method, levels=c("template", "adhoc"))
#data$Method <- revalue(data$Method, c("senor"= "Template-based"))
#data$Method <- revalue(data$Method, c("adhoc"= "Ad hoc"))

p <- ggplot(data = data, aes(x=Yield, fill=Method))
p <- p + geom_histogram(binwidth=5, alpha=0.5, position="identity")

p <- p + facet_wrap( ~ GroupType, scales="free")
p <- p + xlab("Yield") + ylab("#Groups") + ggtitle("Yield - Histogram")
p<- p + xlim(0,45)
p<- p + ylim(0,6)

p <- p+ scale_fill_manual(name="Method: ", breaks=c("template", "adhoc"), labels=c("Template-based", "Ad hoc"), 
                        #  values=c("#0072B2", "#D55E00","#56B4E9","#E69F00"))
                        values=c("#8bb92d", "#8C8C8C","#56B4E9","#E69F00"))

p <- p  + theme(
  plot.margin = margin(20,30,0,10),
  title =element_text(size=35, face='bold'),
  #panel.grid.major = element_line(colour = "grey95", size=0.5),
  panel.border = element_rect(colour = "grey85", size = 0.75, fill=NA),
  #panel.grid.minor = element_blank(),
  #legend.margin=margin(1,1,1,1,"lines"),
  legend.text=element_text(size=35, margin=margin(0,0,0,0)),
  legend.title=element_text(size=27,face="bold"),
  legend.position="bottom",
  legend.key.width=unit(37,"pt"),
  legend.key.height=unit(35,"pt"),
  axis.text.x = element_text(size=27,angle=0,hjust=.5,vjust=.5,face="plain"),
  axis.text.y = element_text(size=30,angle=0,hjust=1,vjust=0,face="plain"),  
  axis.title.x = element_text(size=35,angle=0,hjust=.5,vjust=0,face="plain", margin=margin(20,0,30,0)),
  axis.title.y = element_text(size=35,angle=90,hjust=.5,vjust=.5,face="plain", margin=margin(0,20,0,0)),
  strip.text = element_text(size=35,angle=0,hjust=.5,vjust=.5,face="plain", margin=margin(00,0,0,0)),
  strip.background = element_rect(colour = "grey85", size = 0.75)
)
return(p)
}

lp <- getPlot()
plots <- list(lp)


png(paste(outputFilesPath, outputFileName, ".png", sep=''), width = 1700, height=1000)
multiplot(plotlist = plots, cols=1)
dev.off()
dev.off()

#Shapiro test to check normality
dataGT <- filter(data, data$GroupType=="group"&data$Method=="template")
aGT <- shapiro.test(dataGT$Yield)
print(aGT$p.value)

dataGA <- filter(data, data$GroupType=="group"&data$Method=="adhoc")
aGA <- shapiro.test(dataGA$Yield)
print(aGA$p.value)

dataST <- filter(data, data$GroupType=="single"&data$Method=="template")
aST <- shapiro.test(dataST$Yield)
print(aST$p.value)

dataSA <- filter(data, data$GroupType=="single"&data$Method=="adhoc")
aSA <- shapiro.test(dataSA$Yield)
print(aSA$p.value)

#Wilcox for hypotheses testing

print("Single-Individuals")
wil_ind <- tryCatch( wilcox.test(dataST$Yield,
                                 dataSA$Yield,
                                 paired = F, alternative = "greater" ),
                     error = function(cond){
                       wil_ind$p.value <--1
                       print("Single error testing") 
                       return (wil_ind)
                                          })
print(wil_ind$p.value)
effsize_ind <- cliff.delta(dataST$Yield, dataSA$Yield)
print(effsize_ind$magnitude)
print(effsize_ind$estimate)

  print("Groups")
wil_gr <- tryCatch( wilcox.test(dataGT$Yield,
                                dataGA$Yield,
                                paired = F, alternative = "greater" ),
                    error = function(cond){
                      wil_gr$p.value <--1
                      return (wil_gr)
                    })
print(wil_gr$p.value)
effsize_gr <- cliff.delta(dataGT$Yield, dataGA$Yield)
print(effsize_gr$magnitude)
print(effsize_gr$estimate)


