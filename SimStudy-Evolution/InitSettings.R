#Settings for simulations

initSettings <- function(){
  #Load common libraries
  library(plyr)
  library(dplyr)
  library(MASS)
  #library(effsize)
  library(reshape2)
  library(ggplot2)
  library(Rmisc)
  library(RColorBrewer)
  
  #Initial Settings of variables
  digPrec<<-4 
  resultPath <<- "output/"
  
  #Set styles
  setStyles()
  
  return(NULL)
}

setStyles <- function() {
  
  myBluePalete <<- brewer.pal(n = 9, "Blues")[2:9]
  
  themeP <<-  theme(
    #plot.margin = margin(1.5, 0.25, 0.12, 0.25, "cm"),
    title =element_text(size=27, face="bold"),
    panel.grid.major = element_line(colour = "grey95", size=0.5),
    panel.border = element_rect(colour = "grey85", size = 0.75, fill=NA),
    legend.text=element_text(size=26, margin=margin(0,0,0,0)),
    legend.title=element_text(size=26),
    legend.position="bottom",
    
    legend.key.width=unit(15,"pt"),
    legend.key.height=unit(15,"pt"),
    axis.text.x = element_text(size=26,angle=0,hjust=.5,vjust=.5,face="plain"),
    axis.text.y = element_text(size=26,angle=0,hjust=1,vjust=0,face="plain"),  
    axis.title.x = element_text(size=26,angle=0,hjust=.5,vjust=0,face="plain", margin=margin(10,0,20,0)),
    axis.title.y = element_text(size=26,angle=90,hjust=.5,vjust=.5,face="plain", margin=margin(0,10,0,0))
    # strip.text = element_text(size=35,angle=0,hjust=.5,vjust=.5,face="plain", margin=margin(00,0,0,0)),
    #  strip.backgr
    
  )
  
  themeP2 <<-  theme(
    #plot.margin = margin(1.5, 0.25, 0.12, 0.25, "cm"),
    title =element_text(size=24, face="bold"),
    panel.grid.major = element_line(colour = "grey95", size=0.5),
    panel.border = element_rect(colour = "grey85", size = 0.75, fill=NA),
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    legend.text=element_text(size=22, margin=margin(0,0,0,0)),
    legend.title=element_text(size=22),
    legend.position="bottom",
    legend.key.width=unit(15,"pt"),
    legend.key.height=unit(15,"pt"),
    axis.text.x = element_text(size=26,angle=0,hjust=.5,vjust=.5,face="plain"),
    axis.text.y = element_text(size=26,angle=0,hjust=1,vjust=0,face="plain"),  
    axis.title.x = element_text(size=26,angle=0,hjust=.5,vjust=0,face="plain", margin=margin(10,0,20,0)),
    axis.title.y = element_text(size=26,angle=90,hjust=.5,vjust=.5,face="plain", margin=margin(0,10,0,0))
    # strip.text = element_text(size=35,angle=0,hjust=.5,vjust=.5,face="plain", margin=margin(00,0,0,0)),
    #  strip.backgr
    
  )
  
  myPaletePurples <<-  brewer.pal(n = 9, "Purples")[4:9]
  myPaleteOranges <<- brewer.pal(n=9, "Oranges")[4:9]
  myPaleteBlues <<- brewer.pal(n=9, "Blues")[4:9]
  
  return(NULL)
}


