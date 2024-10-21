


for (i in 3:11) {
  plot1 <- ggplot(chem_data, aes(chem_data[,i])) +
    geom_histogram(binwidth = 0.5) +
    labs(x = colnames(chem_data[i]))
  print(plot1)
  
}





for(i in 3:11){
  plot <- ggplot(comb_data, aes(x = orchard_type, y = comb_data[,i])) +
    stat_boxplot(aes(orchard_type, comb_data[,i]), geom = "errorbar") +
    geom_boxplot(aes(orchard_type, comb_data[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(comb_data[i])) +
    theme(panel.background = element_rect(fill = NA, colour = 'black')) +
    scale_y_continuous(limits = c(min(chem_data[,i]), max(chem_data[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), ".jpeg"), plot = plot)
}

