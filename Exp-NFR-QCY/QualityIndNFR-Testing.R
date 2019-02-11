library(psych)
library(Lahman)
library(dplyr)
library(MASS)
library(effsize)


##########
###For testing hypoteses concerning quality of individual NFRs
###
###
digPrec<-4 
resultPath <- "output/"
inputFileName <-"CumulativeErrorNum" #tak


## Quality Cum - Uncomment proper lines to get the results concerning the intresting characteristic
#### Unambiguity - Interpretation
data <-read.csv(paste("input/",inputFileName,".csv", sep=''), sep=";", header=TRUE, encoding = "UTF-8", skip = 2, nrows=793) #tak
#resultFileName <- "NoMgtCS-analysis-Quality-Cum-I"
resultFileName <- "analysis-Quality-Cum-I"

### Level of Detail - Individual Completeness
#data <-read.csv(paste("./",inputFileName,".csv", sep=''), sep=";", header=TRUE, encoding = "UTF-8", skip = 799, nrows=793) #tak
#resultFileName <- "NoMgtCS-analysis-Quality-Cum-LoD"
#resultFileName <- "analysis-Quality-Cum-LoD"

### Verifiability 
#data <-read.csv(paste("./",inputFileName,".csv", sep=''), sep=";", header=TRUE, encoding = "UTF-8", skip = 2393, nrows=793) #tak
#resultFileName <- "analysis-Quality-Cum-V"
#resultFileName <- "NoMgtCS-analysis-Quality-Cum-V"



ResultByCategory <- c("ID", "Category", "Ind-numgroups(senor)", "Ind-numgroups(adhoc)", "Group-numgroups(senor)", "Group-numgroups(adhoc)", 
                      "Result-single",  "Result-group", "EffSize-single", "Effsize-group", "EffSize-single", "Effsize-group", 
                      "Mean-ai", "Mean-ag","Mean-si", "Mean-sg", "Median-ai", "Median-ag", "Median-si", "Median-sg" , 
                      "Min-ai", "Min-ag", "Min-si", "Min-sg", "Max-ai", "Max-ag", "Max-si", "Max-sg", 
                      "Sum-ai", "Sum-ag", "Sum-si", "Sum-sg", "SD-ai", "SD-ag", "SD-si", "SD-sg",
                      "var-ai", "var-ag", "var-si", "var-sg"
)

categories <- unique(data$Category)

for (i in 1:length(categories)) {
  
  categoryData <- filter(data, data$Category == categories[i])

  #Play with the following data to see if confounding factor - cs/mgmt has influence  
  #categoryDataMgt <- categoryData[grepl("mgt-", categoryData$Spec),] #delete mgt
  #categoryDataGInfSenor <- categoryData[grepl("senor-g-inf-", categoryData$Spec),] #delete cs from groups
  #categoryDataGInfAdhoc <- categoryData[grepl("adhoc-g-inf-", categoryData$Spec),] #delete cs from groups
  #categoryData <- categoryData[!grepl("mgt-", categoryData$Spec),]
  #categoryData <- categoryData[!grepl("g-inf-", categoryData$Spec),]
  
  groupTempl <- filter(categoryData, categoryData$Method == 'senor' & categoryData$GroupType== 'group'& categoryData$Result != 'NA')
  groupAdhoc <- filter(categoryData, categoryData$Method == 'adhoc' & categoryData$GroupType== 'group'& categoryData$Result != 'NA')
  indTempl <- filter(categoryData, categoryData$Method == 'senor' & categoryData$GroupType== 'single'& categoryData$Result != 'NA')
  indAdhoc <- filter(categoryData, categoryData$Method == 'adhoc' & categoryData$GroupType== 'single' & categoryData$Result != 'NA')
  
  
  PValues <- c()
  
  j<-6
  IDgroup <- indAdhoc[1,1]
  groupTemplInterpretaion <- 1 - groupTempl[,j] #turn to correctness
  groupAdhocInterpretation <- 1 - groupAdhoc[,j]
  indTemplInterpretaion <- 1 - as.double(indTempl[,j])
  indAdhocInterpretation <- 1 - as.double(indAdhoc[,j])
  
  # if (length(groupTemplInterpretaion) > 2) {
  # print(paste("groupTempl",i,shapiro.test(groupTemplInterpretaion)$p.value))
  # }
  # if (length(groupAdhocInterpretation) > 2) {
  # print(paste("groupAdhoc",i,shapiro.test(groupAdhocInterpretation)$p.value))
  # }
  # if (length(indTemplInterpretaion) > 2) {
  # print(paste("indTempl",i,shapiro.test(indTemplInterpretaion)$p.value))
  # }
  # if (length(indAdhocInterpretation) > 2) {
  # print(paste("indAdhoc", i,shapiro.test(indAdhocInterpretation)$p.value))
  # }
  
  wil_ind <- 0
  wil_group<-0
  
  if (length(indTemplInterpretaion) == 0) {
    wil_ind$p.value <- -1
  } else {
    wil_ind <- tryCatch(wilcox.test(as.double(indTemplInterpretaion), as.double(indAdhocInterpretation), paired = F, alternative = "greater" ),
                        error = function(cond){
                          wil_ind$p.value <--1
                          return (wil_ind)
                        })
  }
  
  
  if (length(groupTemplInterpretaion) == 0) { 
    wil_group$p.value <- -1
  } else {  
    wil_group <- tryCatch( wilcox.test(groupTemplInterpretaion, groupAdhocInterpretation, paired = F, alternative = "greater" ),
                           error = function(cond){
                             wil_group$p.value <- -1
                             return (wil_group)
                           }) 
  }
 
  
  effsize_ind <- cliff.delta(indTemplInterpretaion, indAdhocInterpretation)
  effsize_group <- cliff.delta(groupTemplInterpretaion, groupAdhocInterpretation)
 
  
 #effsize_indMgt <- cliff.delta(indTemplInterpretaion, 1- categoryDataMgt[,j])
 #effsize_gInfSenor <- cliff.delta(groupTemplInterpretaion, 1- categoryDataGInfSenor[,j])
 #effsize_gInfAdhoc <- cliff.delta(groupAdhocInterpretation, 1 - categoryDataGInfAdhoc[,j])
  
  meanai <- mean(indAdhocInterpretation, na.rm = TRUE)
  meanag <- mean(groupAdhocInterpretation, na.rm = TRUE)
  meansi <- mean(indTemplInterpretaion, na.rm = TRUE)
  meansg <- mean(groupTemplInterpretaion, na.rm = TRUE)
  
  medianai <- median(indAdhocInterpretation, na.rm = TRUE)
  medianag <- median(groupAdhocInterpretation, na.rm = TRUE)
  mediansi <- median(indTemplInterpretaion, na.rm = TRUE)
  mediansg <- median(groupTemplInterpretaion, na.rm = TRUE)
  
  minai <- min(indAdhocInterpretation, na.rm = TRUE)
  minag <- min(groupAdhocInterpretation, na.rm = TRUE)
  minsi <- min(indTemplInterpretaion, na.rm = TRUE)
  minsg <- min(groupTemplInterpretaion, na.rm = TRUE)
  
  maxai <- max(indAdhocInterpretation, na.rm = TRUE)
  maxag <- max(groupAdhocInterpretation, na.rm = TRUE)
  maxsi <- max(indTemplInterpretaion, na.rm = TRUE)
  maxsg <- max(groupTemplInterpretaion, na.rm = TRUE)
  
  sumai <- sum(indAdhocInterpretation, na.rm = TRUE)
  sumag <- sum(groupAdhocInterpretation, na.rm = TRUE)
  sumsi <- sum(indTemplInterpretaion, na.rm = TRUE)
  sumsg <- sum(groupTemplInterpretaion, na.rm = TRUE)
  
  sdai <- sd(indAdhocInterpretation, na.rm = TRUE)
  sdag <- sd(groupAdhocInterpretation, na.rm = TRUE)
  sdsi <- sd(indTemplInterpretaion, na.rm = TRUE)
  sdsg <- sd(groupTemplInterpretaion, na.rm = TRUE)
  
  varai <- var(indAdhocInterpretation, na.rm = TRUE)
  varag <- var(groupAdhocInterpretation, na.rm = TRUE)
  varsi <- var(indTemplInterpretaion, na.rm = TRUE)
  varsg <- var(groupTemplInterpretaion, na.rm = TRUE)
  
  PValues <- rbind(PValues, round(wil_ind$p.value, digPrec), round(wil_group$p.value, digPrec), 
                   as.character(effsize_ind$estimate), as.character(effsize_group$estimate), 
                   as.character(effsize_ind$magnitude), as.character(effsize_group$magnitude), 
                   round(meanai, digPrec), round(meanag, digPrec), round(meansi, digPrec), round(meansg, digPrec),
                   round(medianai, digPrec), round(medianag, digPrec), round(mediansi, digPrec), round(mediansg, digPrec),
                   round(minai, digPrec), round(minag, digPrec), round(minsi, digPrec), round(minsg, digPrec),
                   round(maxai, digPrec), round(maxag, digPrec), round(maxsi, digPrec), round(maxsg, digPrec),
                   round(sumai, digPrec), round(sumag, digPrec), round(sumsi, digPrec), round(sumsg, digPrec),
                   round(sdai, digPrec), round(sdag, digPrec), round(sdsi, digPrec), round(sdsg, digPrec),
                   round(varai, digPrec), round(varag, digPrec), round(varsi, digPrec), round(varsg, digPrec)
  )
  
  ResultByCategory <- rbind(ResultByCategory, c(as.character(IDgroup), as.character(categories[i]), 
                                                length(indTemplInterpretaion), length(indAdhocInterpretation),
                                                length(groupTemplInterpretaion), length(groupAdhocInterpretation), 
                                                PValues))
  
}

resultFile <- paste(resultFileName,format(Sys.time(), "%Y%m%d"), sep='-')
resultFile <- paste(resultFile, "csv", sep='.')
resultFile <- paste(resultPath,resultFile, sep='')

write.matrix(ResultByCategory, file=resultFile, sep=";" )

