makeIndividualDiffPlot <- function (database, questionnaire, questionnaire_title, titletag, savetag, figures_path)
{ # this function does the plot and saves
  
  bs = ddply(database, .(ID,group,questionnaire), summarise, normChangeBehav=mean(normChangeBehav))
  bg = ddply(bs,.(group,questionnaire),summarise, normChangeBehav=mean(normChangeBehav))
  pp <- ggplot(bs, aes(x = questionnaire, y = normChangeBehav, fill = group, color = group)) +
    geom_point() +
    geom_smooth(method=lm, 
                aes(fill = group),# Add linear regression line
                se=T)  +  
    guides(color = "none") +
    theme_bw() +
    labs(
      title = titletag,
      x = questionnaire_title,
      y = "Normalized Change in Behavior"
    ) 

  
  ppp <- pp + theme_classic(base_size = 14, base_family = "Helvetica")+ 
    theme(strip.text.x = element_text(size = 18, face = "bold"), strip.background = element_blank(),
          legend.justification = c(1,1), legend.position = "right",
          legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 18, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"))  
    

  # save the plot in the figures folder
  pdf(file.path(figures_path,savetag))
  print(ppp)
  dev.off()
  
} 