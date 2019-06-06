multiple_rep_plot = function(file, 
                    a_individual = TRUE, b_individual = TRUE, x_individual = FALSE, 
                    a_average = TRUE, b_average = TRUE, x_average = FALSE){
  # create main title
  main_title = bquote("Mulitple Games")
  
  # plot all repetitions for a and b, adding in x for consecutive
  ymax = c(0, 1.1 * max(c(file$a_average[[1]], file$b_average[[1]], file$x_average[[1]])))
  plot(x = -100, y = -100, type = "p", lwd = 1, lty = 1,
       xlim = c(0, max(file$time)), ylim = ymax, xlab = "", ylab = "")
  mtext("Time", side = 1, line = 2, cex = 1.5)
  mtext("Counts", side = 2, line = 2, cex = 1.5)
  mtext(main_title, side = 3, line = 1.5, cex = 2.5)
  if(a_individual){
    for(i in 1:file$num){
      color_a_individual = switch(file$mechanism[i],
                                  "monomolecular" = "deeppink",
                                  "bimolecular" = "dodgerblue",
                                  "catalytic" = "yellow",
                                  "autocatalytic" = "peru",
                                  "consecutive" = "greenyellow",
                                  "equilibrium" = "violetred")
      matlines(x = file$time, y = file$a_reps[[i]], lty = 1, lwd = 0.4, col = color_a_individual)
    }
  }
  if(b_individual){
    for(i in 1:file$num){
      color_b_individual = switch(file$mechanism[i],
                                  "monomolecular" = "red2",
                                  "bimolecular" = "blue",
                                  "catalytic" = "orange",
                                  "autocatalytic" = "brown",
                                  "consecutive" = "green4",
                                  "equilibrium" = "purple4")
      matlines(x = file$time, y = file$b_reps[[i]], lty = 1, lwd = 0.4, col = color_b_individual)
    }
  }
  if (x_individual && "consecutive" %in% file$mechanism){
    for(i in 1:file$num){
      matlines(x = file$time, y = file$x_reps[[i]], lty = 1, lwd = 0.4, col = "darkgreen")
    }
  }
  
  # plot averages for a and b, adding in x for consecutive
  if(a_average){
    for(i in 1:file$num){
      color_a_average = switch(file$mechanism[i],
                               "monomolecular" = "deeppink",
                               "bimolecular" = "dodgerblue",
                               "catalytic" = "yellow",
                               "autocatalytic" = "peru",
                               "consecutive" = "greenyellow",
                               "equilibrium" = "violetred")
      lines(x = file$time, y = file$a_average[[i]], lwd = 2, lty = 2, col = color_a_average)    
    }
  }
  if(b_average){
    for(i in 1:file$num){
      color_b_average = switch(file$mechanism[i],
                               "monomolecular" = "red2",
                               "bimolecular" = "blue",
                               "catalytic" = "orange",
                               "autocatalytic" = "brown4",
                               "consecutive" = "green4",
                               "equilibrium" = "purple4")
      lines(x = file$time, y = file$b_average[[i]], lwd = 2, lty = 2, col = color_b_average)    
    }
  }
  if (x_average ||x_individual && !("consecutive" %in% file$mechanism)){
    for(i in 1:file$num){
      lines(x = file$time, y = file$x_average[[i]], lwd = 2, lty = 2, col = "darkgreen")
    }
  }
  
  # add legend to plot
  legend_text = vector()
  legend_pch = vector()
  legend_color = vector()
  
  if("monomolecular" %in% file$mechanism){
    legend_text = c(legend_text, "monomolecular")
    legend_pch = c(legend_pch, 15)
    legend_color = c(legend_color, "red")
  }
  if("bimolecular" %in% file$mechanism){
    legend_text = c(legend_text, "bimolecular")
    legend_pch = c(legend_pch, 15)
    legend_color = c(legend_color, "blue")
  }
  if("catalytic" %in% file$mechanism){
    legend_text = c(legend_text, "catalytic")
    legend_pch = c(legend_pch, 15)
    legend_color = c(legend_color, "yellow")
  }
  if("autocatalytic" %in% file$mechanism){
    legend_text = c(legend_text, "autocatalytic")
    legend_pch = c(legend_pch, 15)
    legend_color = c(legend_color, "brown")
  }
  if("consecutive" %in% file$mechanism){
    legend_text = c(legend_text, "consecutive")
    legend_pch = c(legend_pch, 15)
    legend_color = c(legend_color, "green")
  }
  if("equilibrium" %in% file$mechanism){
    legend_text = c(legend_text, "equiribrium")
    legend_pch = c(legend_pch, 15)
    legend_color = c(legend_color, "purple")
  }

  legend(x = "top", horiz = TRUE,
         legend = legend_text, pch = legend_pch, pt.cex = 1.5,
         col = legend_color, bty = "n")
  grid(col = "black")
} 

