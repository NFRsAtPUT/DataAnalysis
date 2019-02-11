library(psych)
library(Lahman)
library(dplyr)
library(MASS)
library(effsize)

######
### To test hypotheses concerning set Completeness
###
### If you want to check without management, to see if there is confounding factor influence, play with # lines
digPrec<-4 
fileName <-"SuperSetCumReport"
resultFileName <- "output/analysis-CompletenessCum"

data <-read.csv(paste("input/",fileName,".csv", sep=''), sep=";", header=TRUE, encoding = "UTF-8", skip = 0, nrows=794)

#resultFileName <- "NoMgtCS-analysis-CompletenessCum" 

ResultByCategory <- c("ID", "Category", "Ind-numgroups(templ)", "Ind-numgroups(adhoc)", "Group-numgroups(templ)", "Group-numgroups(adhoc)", 
                      "Result-single",  "Result-group", "EffSize-single", "Effsize-group", "EffSize-single", "Effsize-group", 
                      "Mean-ai", "Mean-ag","Mean-si", "Mean-sg", "Median-ai", "Median-ag", "Median-si", "Median-sg" , 
                      "Min-ai", "Min-ag", "Min-si", "Min-sg", "Max-ai", "Max-ag", "Max-si", "Max-sg", 
                      "Sum-ai", "Sum-ag", "Sum-si", "Sum-sg", "SD-ai", "SD-ag", "SD-si", "SD-sg",
                      "var-ai", "var-ag", "var-si", "var-sg"
)

categories <- unique(data$Category)

for (i in 1:length(categories)) {
  
  categoryData <- filter(data, data$Category == categories[i])
  
  #categoryDataMgt <- categoryData[grepl("mgt-", categoryData$Spec),] #usun mgt
  #categoryDataGInftempl <- categoryData[grepl("senor-g-inf-", categoryData$Spec),] #usun cs from groups
  #categoryDataGInfAdhoc <- categoryData[grepl("adhoc-g-inf-", categoryData$Spec),] #usun cs from groups
  #categoryDataGInftempl <- filter(categoryDataGInftempl, categoryData$Method == 'senor' & categoryData$GroupType== 'group'& categoryData$Result != 'NA')
  #categoryDataGInfAdhoc <- filter(categoryDataGInfAdhoc, categoryData$Method == 'adhoc' & categoryData$GroupType== 'group'& categoryData$Result != 'NA')
  
  #categoryData <- categoryData[!grepl("mgt-", categoryData$Spec),] #usun mgt
  #categoryData <- categoryData[!grepl("g-inf-", categoryData$Spec),] #usun cs from groups
  
  groupTempl <- filter(categoryData, categoryData$Method == 'senor' & categoryData$GroupType== 'group'& categoryData$Result != 'NA')
  groupAdhoc <- filter(categoryData, categoryData$Method == 'adhoc' & categoryData$GroupType== 'group'& categoryData$Result != 'NA')
  indTempl <- filter(categoryData, categoryData$Method == 'senor' & categoryData$GroupType== 'single'& categoryData$Result != 'NA')
  indAdhoc <- filter(categoryData, categoryData$Method == 'adhoc' & categoryData$GroupType== 'single' & categoryData$Result != 'NA')
  
  PValues <- c()
  
  j<-6 #6 to Result
  IDgroup <- indAdhoc[1,1]
  groupTemplCompleteness <-  groupTempl[,j]
  groupAdhocCompleteness <-  groupAdhoc[,j]
  indTemplCompleteness <-  as.double(indTempl[,j])
  indAdhocCompleteness <- as.double(indAdhoc[,j])
  
  
  
  if (length(groupTemplCompleteness) > 2) {
    #print(paste("groupTempl",i,shapiro.test(groupTemplCompleteness)$p.value))
  }
  if (length(groupAdhocCompleteness) > 2 & i >1) {
    #print(paste("groupAdhoc",i,shapiro.test(groupAdhocCompleteness)$p.value))
  }
  if (length(indTemplCompleteness) > 2) {
    #print(paste("indTempl",i,shapiro.test(indTemplCompleteness)$p.value))
  }
  if (length(indAdhocCompleteness) > 2) {
    #print(paste("indAdhoc", i,shapiro.test(indAdhocCompleteness)$p.value))
  }
  
  
  wil_ind <- 0
  wil_group<-0
  
  if (length(indTemplCompleteness) == 0) {
    wil_ind$p.value <- -1
  } else {
    wil_ind <- tryCatch(wilcox.test(as.double(indTemplCompleteness), as.double(indAdhocCompleteness), 
                                    paired = F, alternative = "greater" ),
                                    error = function(cond){
                                    wil_ind$p.value <--1
                                    return (wil_ind)
                                  })
  }
  if (length(groupTemplCompleteness) == 0) { 
    wil_group$p.value <- -1
  } else {  
    wil_group <- tryCatch( wilcox.test(groupTemplCompleteness, groupAdhocCompleteness, paired = F, alternative = "greater" ),
                           error = function(cond){
                             wil_group$p.value <- -1
                             return (wil_group)
                           }) 
  }
  
  effsize_ind <- cliff.delta(indTemplCompleteness, indAdhocCompleteness)
  
  effsize_group <- cliff.delta(groupTemplCompleteness, groupAdhocCompleteness)
  print(paste(i, effsize_group$estimate))
  if (!is.na(effsize_group$estimate)){
    if (effsize_group$estimate < 0) {
      #  print(paste(i, "ind"))
    }
  }else
  {
    # print(i)
  }
  
  
  #effsize_indMgt <- cliff.delta(indTemplCompleteness, categoryDataMgt[,j])
  #effsize_gInftempl <- cliff.delta(groupTemplCompleteness, categoryDataGInftempl[,j])
  #effsize_gInfAdhoc <- cliff.delta(groupAdhocCompleteness, categoryDataGInfAdhoc[,j])
  #print(paste("C", i,":", effsize_indMgt$estimate,"; ", effsize_gInftempl$estimate, "; ", effsize_gInfAdhoc$estimate,";"))
  
  meanai <- mean(indAdhocCompleteness, na.rm = TRUE)
  meanag <- mean(groupAdhocCompleteness, na.rm = TRUE)
  meansi <- mean(indTemplCompleteness, na.rm = TRUE)
  meansg <- mean(groupTemplCompleteness, na.rm = TRUE)
  
  medianai <- median(indAdhocCompleteness, na.rm = TRUE)
  medianag <- median(groupAdhocCompleteness, na.rm = TRUE)
  mediansi <- median(indTemplCompleteness, na.rm = TRUE)
  mediansg <- median(groupTemplCompleteness, na.rm = TRUE)
  
  minai <- min(indAdhocCompleteness, na.rm = TRUE)
  minag <- min(groupAdhocCompleteness, na.rm = TRUE)
  minsi <- min(indTemplCompleteness, na.rm = TRUE)
  minsg <- min(groupTemplCompleteness, na.rm = TRUE)
  
  maxai <- max(indAdhocCompleteness, na.rm = TRUE)
  maxag <- max(groupAdhocCompleteness, na.rm = TRUE)
  maxsi <- max(indTemplCompleteness, na.rm = TRUE)
  maxsg <- max(groupTemplCompleteness, na.rm = TRUE)
  
  sumai <- sum(indAdhocCompleteness, na.rm = TRUE)
  sumag <- sum(groupAdhocCompleteness, na.rm = TRUE)
  sumsi <- sum(indTemplCompleteness, na.rm = TRUE)
  sumsg <- sum(groupTemplCompleteness, na.rm = TRUE)
  
  sdai <- sd(indAdhocCompleteness, na.rm = TRUE)
  sdag <- sd(groupAdhocCompleteness, na.rm = TRUE)
  sdsi <- sd(indTemplCompleteness, na.rm = TRUE)
  sdsg <- sd(groupTemplCompleteness, na.rm = TRUE)
  
  varai <- var(indAdhocCompleteness, na.rm = TRUE)
  varag <- var(groupAdhocCompleteness, na.rm = TRUE)
  varsi <- var(indTemplCompleteness, na.rm = TRUE)
  varsg <- var(groupTemplCompleteness, na.rm = TRUE)
  
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
                                                length(indTemplCompleteness), length(indAdhocCompleteness),
                                                length(groupTemplCompleteness), length(groupAdhocCompleteness), 
                                                PValues))
  
}

resultFile <- paste(resultFileName,format(Sys.time(), "%Y%m%d"), sep='-')
resultFile <- paste(resultFile, "csv", sep='.')


write.matrix(ResultByCategory, file=resultFile, sep=";" )



