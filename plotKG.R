plotKG = function(file, a_plot = TRUE, b_plot = TRUE, x_plot = FALSE, main_title = NULL){
  
  # create vector of which marble counts to plot
  marbles_to_plot = c(a_plot,b_plot,x_plot)
  
  # create vectors for legend text and line colors
  legend_text = c("A", "B", "X")
  line_color = c("blue", "red", "darkgreen")
  
  # create main title
  if(file$mechanism == "monomolecular"){
    main_title = bquote("Monomolecular Mechanism: A" ~ symbol("\256") ~ "B")
  } else if(file$mechanism == "bimolecular"){
    main_title = bquote("Bimolecular Mechanism: 2A" ~ symbol("\256") ~ "2B")
  } else if(file$mechanism == "catalytic"){
    main_title = bquote("Catalytic Mechanism: A + X" ~ symbol("\256") ~ "B + X")
  } else if(file$mechanism == "autocatalytic"){
    main_title = bquote("Autocatalytic Mechanism: A + B" ~ symbol("\256") ~ "2B")
  } else if(file$mechanism == "consecutive"){
    main_title = bquote("Consecutive Mechanism: A" ~ symbol("\256") ~ "X" ~ symbol("\256") ~ "B")
  } else {
    main_title = bquote("Equilibrium Mechanism: A" ~ symbol("\253") ~ "B")
  }
  
  # create plot
  marble_count = data.frame(file$a_count, file$b_count, file$x_count)
  matplot(x = file$time, y = marble_count[, which(marbles_to_plot == TRUE)],
          col = line_color[which(marbles_to_plot == TRUE)],
          type = "l", lwd = 2, lty = 1, 
          ylim = c(0,1.1 * max(marble_count)), 
          xlim = c(0, max(file$time)), 
          xlab = "time", ylab = "count",
          main = main_title)
  legend(x = "top", horiz = TRUE,
         legend = legend_text[which(marbles_to_plot == TRUE)],
         col = line_color[which(marbles_to_plot == TRUE)],
         lwd = 2, lty = 1, bty = "n")
  grid(col = "black")
}