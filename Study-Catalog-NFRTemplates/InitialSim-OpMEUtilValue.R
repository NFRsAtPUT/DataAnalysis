library(psych)
library(Lahman)
require(plyr)
library(dplyr)
library(MASS)
library(effsize)
library(reshape2)
library(ggplot2)
library(Rmisc)
digPrec<-4 

######
### For charts for Initial Run
### Operations
### Maintenance Effort
### Utilization
resultPath <- "output/"
data <-read.csv("input/CatalogNumber.csv", sep=";", header=TRUE, encoding = "UTF-8", dec=".")


dataME <- data
dataME$NoRTsActivePrev<- 83
row <- c(83, dataME$NoRTsActive)
row <- row[-length(row)]
dataME$NoRTsActivePrev <- row
dataME$MEPerc <- (dataME$NumAffectedNoRTs - dataME$NumMatchNoRTs)/dataME$NoRTsActive
dataME$UtilPerc <- (dataME$NumAffectedNoRTs-dataME$NumAddedNoRTs)/dataME$NoRTsActivePrev

#### Operations
dataForBar <- data.frame(SpecOrder = data$SpecOrder, Operations = data$NumAddedNoRTs, Type="Added")
dataForBar <- rbind(dataForBar, data.frame(SpecOrder = data$SpecOrder, Operations = data$NumAffectedNoRTs - data$NumAddedNoRTs - data$NumMatchNoRTs, Type="Modified"))
dataForBar <- rbind(dataForBar, data.frame(SpecOrder = data$SpecOrder, Operations = data$NumMatchNoRTs, Type="Matched"))
dataForBar <- rbind(dataForBar, data.frame(SpecOrder = data$SpecOrder, Operations = data$NoRTsActive - data$NumAffectedNoRTs, Type="UnTouched"))
p<- ggplot(data = dataForBar) 
p <- p + geom_bar(data=dataForBar, aes(x=SpecOrder, y=Operations, label = Operations, fill=Type), stat = "identity")
p <- p+  geom_text(size = 3, aes( x=SpecOrder, y=Operations, label = round(Operations, digits=1)),
                   position=position_stack(), vjust=-0.75)

p <- p + ggtitle("Catalog operations") + ylab("#NFR Templates") + xlab("Projects")


p <- p + scale_fill_manual(name="Type:", breaks=c("Matched", "Modified", "Added", "UnTouched"), labels=c( "Perfect","Modified", "Added", "Sleeping"), 
                           #  values=c("#0072B2", "#D55E00","#56B4E9","#E69F00"))
                           values=c("#56B4E9","#E69F00","#8bb92d", "#bbbbbb"))


p <- p  + theme(
  title =element_text(size=15, face='bold'),
  panel.grid.major = element_line(colour = "grey95", size=0.5),
  panel.border = element_rect(colour = "grey85", size = 0.75, fill=NA),
  legend.text=element_text(size=13, margin=margin(0,0,0,0)),
  legend.title=element_text(size=15,face="bold"),
  legend.position="bottom",
  legend.key.width=unit(15,"pt"),
  legend.key.height=unit(15,"pt"),
  axis.text.x = element_text(size=10,angle=0,hjust=.5,vjust=.5,face="plain"),
  axis.text.y = element_text(size=10,angle=0,hjust=1,vjust=0,face="plain"),  
  axis.title.x = element_text(size=15,angle=0,hjust=.5,vjust=0,face="plain", margin=margin(10,0,20,0)),
  axis.title.y = element_text(size=15,angle=90,hjust=.5,vjust=.5,face="plain", margin=margin(0,10,0,0))
  # strip.text = element_text(size=35,angle=0,hjust=.5,vjust=.5,face="plain", margin=margin(00,0,0,0)),
  #  strip.background = element_rect(colour = "grey85", size = 0.75)
)
p
pdf(paste("./output/Operations", ".pdf", sep=''), width = 10, height = 7)
multiplot(plotlist = list(p), cols=1)
dev.off()


###Maintainance Effort
dataForBar <- data.frame(SpecOrder = data$SpecOrder, Operations = data$NumAffectedNoRTs - data$NumMatchNoRTs, Type="AddedOrModified", Match=data$NumMatchNoRTs)

#Check tendency
fit <- lm(data = dataForBar, Operations ~ SpecOrder)
summary(fit)

p <- ggplot(data = dataForBar) 
reg <-data.frame(point = c(0,42), value = c(24.75, 1.39))
p <- p + geom_line(data = reg, aes(x=point, y=value), color="grey45", size=1.0)
p <- p + geom_bar(data=dataForBar, aes(x=SpecOrder, y=Operations, label = Operations, fill=Type), stat = "identity")
p <- p + ylab("Maintenance Effort") + xlab("Projects")
p<- p + ylim(0,65)
p<- p + xlim(0,75)

p <- p + scale_fill_manual(name="Type:", breaks=c("Matched", "Modified", "Added", "UnTouched"), 
                           labels=c( "Matched","Indirectly derived", "Added", "UnTouched"), 
                           #  values=c("#0072B2", "#D55E00","#56B4E9","#E69F00"))
                           values=c("grey70", "#8bb92d", "grey55","grey65"))
newx<-seq(1,100) 
prd<-predict(fit,newdata=data.frame(SpecOrder=newx),interval = c("confidence"), 
             level = 0.95)
dataS <- data.frame(SpecOrder = newx, lwr = prd[,2], upr = prd[,3])

p <- p + geom_line(data = filter(dataS, dataS$lwr>=0), aes(x=SpecOrder, y=lwr), color="grey35", size=0.75, linetype="dotted")
p <- p + geom_line(data = filter(dataS, dataS$upr>=0), aes(x=SpecOrder, y=upr), color="grey35", size=0.75, linetype="dotted")
points <- data.frame(SpecOrder = as.numeric(c(34,43,72)), val = c(0,0,0), stringsAsFactors = FALSE)
p <- p + geom_point(data = points, aes(x=SpecOrder, y=val), color="grey35", size=3)
min<- as.character(expression(P[ME]^{min}))
max<- as.character(expression(P[ME]^{max}))
pred<- as.character(expression(P[ME]^{pred}))

p <- p + geom_text(data = points, aes(x=SpecOrder, y=val, label = c(min,
                                                                    pred,
                                                                    max)), color="grey25", 
                   position=position_stack(), vjust=0.99, hjust=0.25, parse=TRUE, size=9)
p <- p + ylim(-10,80) + scale_y_continuous(labels=c(0,20,40,60))
p

pdf(paste("./output/MaintainanceEffort", ".pdf", sep=''), width = 19, height = 10)
#png(paste("./output/MaintainanceEffort", ".png", sep=''), width = 800, height = 1000)
multiplot(plotlist = list(p), cols=1)
dev.off()




### For Maturity Point of ME
#meffort <- data.frame(SpecOrder=1, Intercept =0, Coef = 0, MPV =0, MPVfit=0,
#                     lwrMinSO = 0, lwrMinLwr = 0, lwrMinUpr = 0, 
#                     uprMinSO = 0, uprMinLwr = 0, uprMinUpr = 0,
#                     levelPred = 0)

# levelPred <- 0.95
# for (i in 2:42) {
#   fit <-
#     lm(Operations ~ SpecOrder,
#        data = filter(dataForBarME, dataForBarME$SpecOrder <= i))
#   newx <- seq(0, 1000000)
#   prd <-
#     predict(
#       fit,
#       newdata = data.frame(SpecOrder = newx),
#       interval = c("confidence"),
#       level = levelPred
#     )
#   dataS <-
#     data.frame(
#       SpecOrder = newx,
#       fit = prd[, 1],
#       lwr = prd[, 2],
#       upr = prd[, 3]
#     )
#   #p <- ggplot(data = dataS)
#   #p <- p + geom_line(data = dataS, aes(x=SpecOrder, y=fit), color="red", size=1.0)
#   #p <- p + geom_line(data = dataS, aes(x=SpecOrder, y=lwr), color="blue", size=1.0)
#   #p <- p + geom_line(data = dataS, aes(x=SpecOrder, y=upr), color="green", size=1.0)
#   #p
#   lwrMin <- filter(dataS, dataS$lwr <= 0)[1, ]
#   
#   uprMin <- filter(dataS, dataS$upr <= 0)[1, ]
#   
#   
#   if (nrow(lwrMin) > 0 && nrow(uprMin) > 0)
#   {
#     meffort <-
#       rbind(
#         meffort,
#         data.frame(
#           SpecOrder = i,
#           Intercept = fit$coefficients[[2]],
#           Coef = fit$coefficients[[1]],
#           MPV = (filter(dataS, dataS$fit <= 0)[1, ])$SpecOrder,
#           MPVfit = (filter(dataS, dataS$fit <= 0)[1, ])$fit,
#           lwrMinSO = lwrMin$SpecOrder,
#           lwrMinLwr = lwrMin$lwr,
#           lwrMinUpr = lwrMin$upr,
#           uprMinSO = uprMin$SpecOrder,
#           uprMinLwr = uprMin$lwr,
#           uprMinUpr = uprMin$upr,
#           levelPred = levelPred
#         )
#       )
#   }
# }
# meffort
# 
# write.csv(meffort, file = "a.csv")





#### Effectiveness-Utilization
dataForBar <- data.frame(SpecOrder = data$SpecOrder, Operations = data$NumAffectedNoRTs  - data$NumAddedNoRTs, Type="MatchedModified")
dataForBar <- rbind(dataForBar, data.frame(SpecOrder = data$SpecOrder, 
                                           Operations = c(83, data[1:nrow(data)-1,2])
                                           #$NoRTsActive - data$NumAffectedNoRTs + data$NumAddedNoRTs
                                           , Type="Rest"))
dataForBar$Operations[dataForBar$Type=="Rest"] <- dataForBar$Operations[dataForBar$Type=="Rest"] - dataForBar$Operations[dataForBar$Type=="MatchedModified"]


dataForBarTxt <- data.frame(SpecOrder = data$SpecOrder, 
                            Operations= data$NumAffectedNoRTs - data$NumAddedNoRTs, 
                            CatalogSize = round((data$NumAffectedNoRTs - data$NumAddedNoRTs)/data$NoRTsActive*100,digits=1), 
                            NoRTsActive = data$NoRTsActive, Type="Modified"
)
dataForBarTxt$NoRTsActive2 <-c(83, data[1:nrow(data)-1,2] )

#Check the tendency
fit <- lm(data = dataForBarTxt, Operations ~ SpecOrder)
summary(fit)

p<- ggplot(data = dataForBar) 

p <- p + geom_bar(data=dataForBar, aes(x=SpecOrder, y=Operations, label = Operations, fill=Type), stat = "identity") #+ coord_flip()

p <- p+  geom_text(data = filter(dataForBarTxt, Operations >= 0), size = 5, aes( x=SpecOrder, y=Operations+9, 
                                                                                 # label = paste("(",round(Operations, digits=1),";", CatalogSize, "%)"), parse=TRUE, angle=90, hjust=1), vjust=0.5, 
                                                                                 label = Operations, hjust=0.5),  
                   colour="black", fontface = "bold", family="URWGothic")

p <- p+  geom_text(data = filter(dataForBarTxt, Operations >= 0), size = 5, aes( x=SpecOrder, y=NoRTsActive2+9, 
                                                                                 # label = paste("(",round(Operations, digits=1),";", CatalogSize, "%)"), parse=TRUE, angle=90, hjust=1), vjust=0.5, 
                                                                                 label = NoRTsActive2, hjust=0.5), 
                   colour="black", fontface = "bold", family="URWGothic")


p <- p + ylab("#NFR templates") + xlab("Projects")
p<- p + ylim(0,410)

p <- p + scale_fill_manual(name="", breaks=c("MatchedModified",  "Rest"), 
                           labels=c( "Perfect and Modfified NFR Templates", "Catalog Size"), 
                           #  values=c("#0072B2", "#D55E00","#56B4E9","#E69F00"))
                           values=c("grey60", "grey80", "#8bb92d", "grey65"))


p <- p  + theme(
  title =element_text(size=15, face='bold'),
  panel.grid.major = element_line(colour = "grey95", size=0.5),
  panel.border = element_rect(colour = "grey85", size = 0.75, fill=NA),
  legend.text=element_text(size=25, margin=margin(0,0,0,0)),
  legend.title=element_text(size=14,face="bold"),
  legend.position="bottom",
  legend.key.width=unit(25,"pt"),
  legend.key.height=unit(25,"pt"),
  axis.text.x = element_text(size=25,angle=0,hjust=.5,vjust=.5,face="plain"),
  axis.text.y = element_text(size=25,angle=0,hjust=1,vjust=0,face="plain"),  
  axis.title.x = element_text(size=30,angle=0,hjust=.5,vjust=0,face="plain", margin=margin(10,0,20,0)),
  axis.title.y = element_text(size=30,angle=90,hjust=.5,vjust=.5,face="plain", margin=margin(0,10,0,0))
  # strip.text = element_text(size=35,angle=0,hjust=.5,vjust=.5,face="plain", margin=margin(00,0,0,0)),
  #  strip.background = element_rect(colour = "grey85", size = 0.75)
)
p
pdf(paste("./output/Utilization", ".pdf", sep=''), width = 19, height = 8)
multiplot(plotlist = list(p), cols=1)

dev.off()

########
### Value
data <-read.csv("input/TeachingNFRs.csv", sep=";", header=TRUE, encoding = "UTF-8", dec=".")
numOfNFRs <-read.csv("input/allSpecs.csv", sep=";", header=TRUE, encoding = "UTF-8", dec=".")
sum(numOfNFRs$AllIncluded)
data <- data[,1:7] 

data$NumAffectedNoRTs[data$NumAffectedNoRTs > 1] <- 1 
data$NumAddedNoRTs[data$NumAddedNoRTs > 1] <- 1 
dataAggr <- aggregate(.~SpecOrder, data = data, FUN=sum )
dataAggr <- merge(dataAggr, numOfNFRs[,1:6], by = "SpecOrder", all=TRUE)

dataAggr$PercAffected <- dataAggr$NumAffectedNoRTs/dataAggr$AllIncluded
dataAggr$PercAdded <- dataAggr$NumAddedNoRTs/dataAggr$AllIncluded
dataAggr$PercMatch <- dataAggr$NumMatchNoRTs/dataAggr$AllIncluded

dataForBar <- data.frame(SpecOrder = dataAggr$SpecOrder, Type = "Adding", Result = dataAggr$PercAdded)
dataForBar <- rbind(dataForBar, data.frame(SpecOrder= dataAggr$SpecOrder, Type = "Modification" ,Result = dataAggr$PercAffected-dataAggr$PercAdded - dataAggr$PercMatch))
dataForBar <- rbind(dataForBar, data.frame(SpecOrder= dataAggr$SpecOrder, Type = "Match" ,Result = dataAggr$PercMatch))


dataForBarU <- data.frame(SpecOrder = dataAggr$SpecOrder, Type = "Match", Result = dataAggr$PercMatch)
dataForBarU <- rbind(dataForBarU, data.frame(SpecOrder= dataAggr$SpecOrder, Type = "Modification" ,
                                             Result = dataAggr$PercAffected-dataAggr$PercAdded - dataAggr$PercMatch))
dataForBarUTxt <- data.frame(SpecOrder = dataAggr$SpecOrder, Type = "MatchModified", Result = dataAggr$PercAffected-dataAggr$PercAdded)

p<- ggplot(data = dataForBarU) 
p <- p + geom_bar(data=dataForBarUTxt, aes(x=factor(SpecOrder, levels=seq(1,42)), y=Result*100, fill=Type, label = Result), stat = "identity")

mean(dataForBarUTxt$Result)
median(dataForBarUTxt$Result)
summary(dataForBarUTxt$Result)

p <- p  + ylab("Value [%]") + xlab("Projects") + labs(title="Value")
p<- p + ylim(0,100)
p <- p + scale_fill_manual(name="Type of deriviation:", breaks=c("MatchModified", "Modification" ), labels=c("Direct and Indirect", "Indirect"), 
                           #  values=c("#0072B2", "#D55E00","#56B4E9","#E69F00"))
                           # values=c( myBluePalte[7], "grey70", "grey70", "grey70","grey70"))
                           values=c( "grey70", "grey70", "grey70", "grey70","grey70"))

p <- p + geom_smooth(data = dataForBarUTxt, 
                     aes(x=SpecOrder, y=Result*100), color="grey45", 
                     size=1.0, method = "lm",
                     se=FALSE)

p
#p <- p + geom_line(data = reg, aes(x=point, y=value), color="grey35", size=1.1)
#p <- p + geom_abline(data = dataForBarUTxt, aes(x=SpecOrder, y=Result),
#                     intercept=0.58 , slope= 0.01, color ="red")


# p <- p  + theme(
#   plot.margin = margin(1.5, 0.25, 0.12, 1.5, "cm"),
#   title =element_text(size=27, face='bold'),
#   panel.grid.major = element_line(colour = "grey95", size=0.5),
#   panel.border = element_rect(colour = "grey85", size = 0.75, fill=NA),
#   #legend.text=element_text(size=13, margin=margin(0,0,0,0)),
#   #legend.title=element_text(size=15,face="bold"),
#   legend.position="no",
#   #legend.key.width=unit(15,"pt"),
#   #legend.key.height=unit(15,"pt"),
#   axis.text.x = element_text(size=26,angle=0,hjust=.5,vjust=.5,face="plain"),
#   axis.text.y = element_text(size=26,angle=0,hjust=1,vjust=0,face="plain"),  
#   axis.title.x = element_text(size=26,angle=0,hjust=.5,vjust=0,face="plain", margin=margin(10,0,20,0)),
#   axis.title.y = element_text(size=26,angle=90,hjust=.5,vjust=.5,face="plain", margin=margin(0,10,0,0))
#   # strip.text = element_text(size=35,angle=0,hjust=.5,vjust=.5,face="plain", margin=margin(00,0,0,0)),
#   #  strip.background = element_rect(colour = "grey85", size = 0.75)
# )

fit <- lm(Result*100 ~ SpecOrder, data=filter(dataForBarUTxt, dataForBarUTxt$SpecOrder >0))
newx<-seq(1,100) 
prd<-predict(fit,newdata=data.frame(SpecOrder=newx),interval = c("confidence"), 
             level = 0.95)

dataS <- data.frame(SpecOrder = newx, lwr = prd[,2], upr = prd[,3])
p <- p + geom_line(data = filter(dataS, dataS$lwr<=100), aes(x=SpecOrder, y=lwr), color="grey35", size=0.75, linetype="dotted")
p <- p + geom_line(data = filter(dataS, dataS$upr<=100), aes(x=SpecOrder, y=upr), color="grey35", size=0.75, linetype="dotted")
points <- data.frame(SpecOrder = as.numeric(c(32,38,47)), val = c(100,100,100), stringsAsFactors = FALSE)
p <- p + geom_point(data = points, aes(x=SpecOrder, y=val), color="grey35", size=3)
min<- as.character(expression(P[Value]^{min}))
max<- as.character(expression(P[Value]^{max}))
pred<- as.character(expression(P[Value]^{pred}))
p <- p + geom_text(data = points, aes(x=SpecOrder, y=val, label = c(min,
                                                                    pred,
                                                                    max)), color="grey25", 
                   position=position_stack(), vjust=-0.25, hjust=1, parse=TRUE, size=9)

p

### For MP in Value
# values <- data.frame(SpecOrder=1, Intercept =0, Coef = 0, MPV =0, MPVfit=0,
#                      lwrMinSO = 0, lwrMinLwr = 0, lwrMinUpr = 0, 
#                      uprMinSO = 0, uprMinLwr = 0, uprMinUpr = 0, levelPred = 0)
# 
# for (i in 2:42){
#   fit <- lm(Result*100 ~ SpecOrder, data=filter(dataForBarUTxt, dataForBarUTxt$SpecOrder <= i))
#   newx<-seq(0,1000000) 
# levelPred <- 0.95
#   prd<-predict(fit,newdata=data.frame(SpecOrder=newx),interval = c("confidence"), 
#                level = levelPred)
#   dataS <- data.frame(SpecOrder = newx, fit = prd[,1], lwr = prd[,2], upr = prd[,3])
#   lwrMin <- filter(dataS, dataS$lwr >= 100 )[1,]
#   uprMin <- filter(dataS, dataS$upr >= 100 )[1,]
#   if (nrow(lwrMin) > 0 && nrow(uprMin) > 0)
# {
#   values <- rbind(values, data.frame(SpecOrder=i, Intercept = fit$coefficients[[2]], Coef = fit$coefficients[[1]], 
#                                      MPV = (filter(dataS, dataS$fit >=100)[1,])$SpecOrder, 
#                                      MPVfit = (filter(dataS, dataS$fit >=100)[1,])$fit,
#                                      lwrMinSO = lwrMin$SpecOrder, lwrMinLwr = lwrMin$lwr, lwrMinUpr = lwrMin$upr, 
#                                      uprMinSO = uprMin$SpecOrder, uprMinLwr = uprMin$lwr, uprMinUpr = uprMin$upr, levelPred = levelPred))  
#   } 
# }
# values
# write.csv(values, file="value-MP.csv")

#PhD Thesis:
#p <- p + labs(title="A. Initial evolution") #Title for PhD Thesis
#p <- p + themeP2 + theme(legend.position="no") #themeP2 - the one used in PhD Thesis
#dev.off()
#dev.off()
pdf(paste("./output/Value", ".pdf", sep=''), width = 19, height = 10)
#png(paste("./output/Value", ".png", sep=''), width = 800, height = 1000)
multiplot(plotlist = list(p), cols=1)
#print(p)
dev.off()
#dev.off()



#For cheking the tendency in Value
fit <- lm(Result*100 ~ SpecOrder, data=filter(dataForBarUTxt, dataForBarUTxt$SpecOrder >0))
plot(dataForBarUTxt$Result*100 ~ dataForBarUTxt$SpecOrder, col = "gray", xlim=c(-10, 100),  ## with c()
     ylim=c(0,100))

newx<-seq(1,100) 
prd<-predict(fit,newdata=data.frame(SpecOrder=newx),interval = c("confidence"), 
             level = 0.50)
prd

points(prd[1:42,1] ~ dataForBarTxt$SpecOrder, type = "l", col = "blue")
lines(newx,prd[,2],col="red",lty=2) 
lines(newx,prd[,3],col="red",lty=2) 


resultFrame <- data.frame(SpecOrder= dataForBarUTxt$SpecOrder[2], Value = dataForBarUTxt$Result[2],
                          A =coef(fit)[[1]], B = coef(fit)[[2]], pA=
                            coef(summary(fit))[, "Pr(>|t|)"][[1]],
                          pB=coef(summary(fit))[, "Pr(>|t|)"][[2]],
                          r2=summary(fit)$r.squared,
                          ar2=summary(fit)$adj.r.squared)
for (i in 2:41) {
  fit <- lm(Result*100 ~ SpecOrder, data=filter(dataForBarUTxt, dataForBarUTxt$SpecOrder <= i))                                            
  resultFrame <- rbind(resultFrame,data.frame(SpecOrder= dataForBarUTxt$SpecOrder[i], Value = dataForBarUTxt$Result[i],
                                              A =coef(fit)[[1]], B = coef(fit)[[2]], pA=
                                                coef(summary(fit))[, "Pr(>|t|)"][[1]],
                                              pB=coef(summary(fit))[, "Pr(>|t|)"][[2]],
                                              r2=summary(fit)$r.squared,
                                              ar2=summary(fit)$adj.r.squared))
}

fit <- lm(Result*100 ~ SpecOrder, data=dataForBarUTxt)
summary(fit)
coef(fit)[[1]]
coef(fit)[[2]]
coef(summary(fit))[, "Pr(>|t|)"][[1]]
coef(summary(fit))[, "Pr(>|t|)"][[2]]
summary(fit)$r.squared
summary(fit)$adj.r.squared

