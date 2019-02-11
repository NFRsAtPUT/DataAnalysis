library(psych)
library(Lahman)
library(dplyr)
library(MASS)
library(effsize)
library(reshape2)
library(ggplot2)
library(Rmisc)

##########
###For printing a chart with quality of individual NFRs
###
###
digPrec<-4 
resultPath <- "output/"
inputFileName <- "CumulativeErrorNum"
outputFileName <- "output/QualityIndNFR-CumChart"
inputFilePath <- paste("input/",inputFileName,".csv", sep='')

### Function
### path - path to inputfile
### start - row to start reading input file
### rows - num of rows to read
### title - title of plot
### min and max - for setting min and max values of y-axis
### ylabel - label for y-axis
### showLegend -  do you want to show legend at the bootm
createPlot <- function(path, start, rows, title, min = 0, max, ylabel, showLegend = 1) {
  
  data <-read.csv(path, sep=";", header=TRUE, encoding = "UTF-8", skip = start, nrows = rows)
  
  data <- filter(data, data$Category != "?atwo?? zamiany")
  data <- filter(data, data$Category != "?atwo?? nauczenia si?")
  data <- filter(data, data$Category != "?atwo?? adaptacji")
  data <- filter(data, data$Category != "?atwo?? instalacji")
  data <- filter(data, data$Category != "Wsp??istnienie")
  data <- filter(data, data$Category != "Interoperacyjno??")
  data <- filter(data, (data$GroupType =="group" & data$Category != "Zu?ycie zasob?w") | (data$GroupType=="single") )
  data <- filter(data, data$GroupType=="group" & data$Category != "Charakterystyka czasowa" | (data$GroupType=="single"))
  
  
  columns <- c(1,2,3,4,5,6
               #11,
               #17
               )
  data <- data[,columns]
  
  data$GroupType <- factor(data$GroupType, levels=c("single", "group"))
  data$GroupType <- revalue(data$GroupType, c("group"= "Teams"))
  data$GroupType <- revalue(data$GroupType, c("single"= "Individuals"))
  data$Method <- factor(data$Method, levels=c("senor", "adhoc"))
  data$ID <- factor(data$ID, levels=c("C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20"))
  data$Result <- 1 - data$Result
  
  
  p <- ggplot(data = data, aes(x=ID, y=Result))
  
  ### next three lines to determine if cs,mgmt has difference
  #dataMgt <- data[grepl("mgt", data$Spec) == TRUE | grepl("g-inf", data$Spec) == TRUE ,]
  #dataCS <- data[!grepl("mgt", data$Spec) == TRUE & !grepl("g-inf", data$Spec) == TRUE ,]
  #p <- p + geom_boxplot(data = dataCS, aes(color=Method, fill=Method), varwidth=FALSE, outlier.shape = NA)
  
  p <- p + geom_boxplot(data = data, aes(color=Method, fill=Method), varwidth=FALSE, outlier.shape = NA)
  
  ### next three lines to determine if cs,mgmt has difference
  #p <- p + geom_line(data = dataMgt, aes( group=Spec),color="red")
  #p <- p + geom_text(data = dataMgt, aes( group=Spec, label=Spec),color="red")
  #p <- p + geom_text(data = dataCS, aes( group=Spec, label=Spec),color="blue")
  
  
  # if you want color for points replace group with colour=Label
  #p <- p + geom_point(aes(y=Category, group=Method), position = position_dodge(width=0.75))
  p <- p + facet_wrap( ~ GroupType, scales="free")
  #p <- p + ylim(c(min,max))
  p <- p + xlab("Category ID") + ylab(title) + ggtitle(title)
  #p <- p + guides(fill=guide_legend(title="Method"))
  p <- p + scale_colour_identity()
  p <- p+ scale_fill_manual(name="Method: ", breaks=c("senor", "adhoc"), labels=c("Template-based", "Ad hoc"), 
                            #values=c("#8bb92d", "#8C8C8C","#56B4E9","#E69F00"))
                            #values=c("#0072B2", "#D55E00","#56B4E9","#E69F00"))
                            values=c("grey60","white","#8bb92d", "#8C8C8C","#56B4E9","#E69F00"))
  p  <- p + scale_colour_manual(name="Method: ", breaks=c("senor", "adhoc"), values=c("grey45","grey60"), 
                                labels=c("Template-based", "Ad hoc"))
  
  labels <- c("C1",  "C3","C5","C7","C9","C11","C13","C15","C17","C19")
  labels1 <- c(expression(c[1]), expression(c[3]),expression(c[5]),expression(c[7]),expression(c[9]),expression(c[11]),expression(c[13]),expression(c[15]),expression(c[17]),expression(c[19]))
  p <- p + scale_x_discrete(breaks=labels, labels=labels1)
  
  
  p <- p +  theme(
    plot.margin = margin(20,30,5,10),
    title =element_text(size=55, face='bold'),  
    panel.border = element_rect(colour = "grey85", size = 0.75, fill=NA),
    #legend.margin=margin(1,1,1,1,"lines"),
    legend.text=element_text(size=60),
    legend.title=element_text(size=40,face="bold"),
    
    legend.key.width=unit(75,"pt"),
    legend.key.height=unit(75,"pt"),
    axis.text.x = element_text(size=60,angle=0,hjust=.5,vjust=.5,face="plain"),
    axis.text.y = element_text(size=55,angle=0,hjust=1,vjust=0,face="plain"),  
    axis.title.x = element_text(size=50,angle=0,hjust=.5,vjust=0,face="plain", margin=margin(25,0,5,0)),
    axis.title.y = element_text(size=50,angle=90,hjust=.5,vjust=.5,face="plain", margin=margin(0,30,0,0)),
    strip.text = element_text(size=55,angle=0,hjust=.5,vjust=.5,face="plain", margin=margin(00,0,0,0)),
    strip.background = element_rect(colour = "grey85", size = 0.75)
  )
  
  if (showLegend==0)
  {
    p <- p + theme(legend.position="none")
  } else
  {
    p <- p + theme(legend.position="bottom")
  }
  
  return(p)
}



plots <- list(
  createPlot(inputFilePath, 2, 793, title="(1) Unambiguity Ratio", min=0, max=1, "Quality", showLegend=0)
  ,
  createPlot(inputFilePath, 799, 793, title="(2) Inidivual Completeness Ratio", min=0, max=1, "Quality", showLegend=0)
  ,
  createPlot(inputFilePath, 2393, 793, title="(3) Verifiability Ratio", min=0, max=1, "Quality", showLegend=1)
)



pdf(paste(outputFileName, ".pdf", sep=''), width = 25, height=60)
multiplot(plotlist = plots, cols=1)
dev.off()
dev.off()




