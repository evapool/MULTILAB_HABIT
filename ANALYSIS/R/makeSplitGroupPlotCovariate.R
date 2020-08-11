makeSplitGroupPlotCovariate <- function (database, titletag, savetag, figures_path)
{ # this function does the plot and saves
  
  bs = ddply(database, .(ID,group,traitGroup), summarise, valueDiff=mean(valueDiff))
  bg = ddply(bs,.(group,traitGroup),summarise, valueDiff=mean(valueDiff))
  pp <- ggplot(bs, aes(x = group, y = valueDiff, fill = group, color = group)) +
    geom_point() +
    geom_bar(data =bg, stat = "identity", alpha = .3) +
    #geom_violin(aes(color = group, fill = group), alpha = .3, size = .1) +
    facet_grid(~ traitGroup) +
    
    theme_bw() +
    labs(
      title = '',
      x = titletag, 
      y = "Value diff in Behavior"
    )
  
  ppp <- pp + theme_linedraw(base_size = 14, base_family = "Helvetica")+ 
    theme(strip.text.x = element_text(size = 18, face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.justification = c(1,1), legend.position = "right",
          legend.text = element_text(size = 14),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x = element_text(size = 18, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"))  
  
  # save the plot in the figures folder
  pdf(file.path(figures_path,savetag))
  print(ppp)
  dev.off()
  
} 