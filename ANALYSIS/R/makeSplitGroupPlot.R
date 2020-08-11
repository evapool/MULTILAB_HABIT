makeSplitGroupPlot <- function (database, titletag, savetag, figures_path)
{ # this function does the plot and saves
  
  bs = ddply(database, .(ID,group,traitGroup), summarise, normChangeBehav=mean(normChangeBehav))
  bg = ddply(bs,.(group,traitGroup),summarise, normChangeBehav=mean(normChangeBehav))
  er = ddply(bs,.(group,traitGroup),summarise, normChangeBehav=sd(normChangeBehav)/sqrt(length(normChangeBehav)))
  bg$sd <- er$normChangeBehav
  
  pp <- ggplot(bs, aes(x = group, y = normChangeBehav, fill = group, color = group)) +
    geom_point(data=bs, stat = "identity", size = 2, 
               position = position_jitterdodge(jitter.width = 0.3, jitter.height = 0.3),alpha = .3) +
    geom_bar(data =bg, stat = "identity", alpha = .5) +
    geom_errorbar(data = bg, stat = "identity", aes( ymin = normChangeBehav - sd , ymax = normChangeBehav + sd),width=.1,size=0.7) +
    #geom_violin(aes(color = group, fill = group), alpha = .3, size = .1) +
    facet_grid(~ traitGroup) +

    theme_bw() +
    labs(
      title = '',
      x = titletag, 
      y = "Normalized Change in Behavior"
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