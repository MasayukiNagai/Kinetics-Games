rep_plot = function(file, 
                    a_individual = TRUE, b_individual = TRUE, x_individual = FALSE, 
                    a_average = TRUE, b_average = TRUE, x_average = FALSE){
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
  
# plot all repetitions for a and b, adding in x for consecutive
  ymax = c(0, 1.1 * max(c(file$a_average, file$b_average, file$x_average)))
  plot(x = -100, y = -100, type = "p", lwd = 1, lty = 1,
       xlim = c(0, max(file$time)), ylim = ymax, xlab = "", ylab = "")
  mtext("Time", side = 1, line = 2, cex = 1.5)
  mtext("Counts", side = 2, line = 2, cex = 1.5)
  mtext(main_title, side = 3, line = 1.5, cex = 2.5)
  if(a_individual){
    matlines(x = file$time, y = file$a_reps, lty = 1, lwd = 1, col = "cyan")
  }
  if(b_individual){
    matlines(x = file$time, y = file$b_reps, lty = 1, lwd = 1, col = "pink")
  }
  if (x_individual && file$mechanism == "consecutive"){
    matlines(x = file$time, y = file$x_reps, lty = 1, lwd = 1, col = "palegreen")
  }
  
# plot averages for a and b, adding in x for consecutive
  if(a_average){
    lines(x = file$time, y = file$a_average, lwd = 2, lty = 2, col = "darkblue")    
  }
  if(b_average){
    lines(x = file$time, y = file$b_average, lwd = 2, lty = 2, col = "red")    
  }
  if (x_average ||(x_individual && file$mechanism != "consecutive")){
    lines(x = file$time, y = file$x_average, lwd = 2, lty = 2, col = "darkgreen")
  }
  
# add legend to plot
  if (file$mechanism == "consecutive"){
    legend_text = c("A", "B", "X", "averages")
    legend_pch = c(15, 15, 15, NA)
    legend_color = c("cyan", "pink", "palegreen", "black")
    legend_lty = c(NA, NA, NA, 2)
    legend_lwd = c(NA, NA, NA, 2)
  } else {
    legend_text = c("A", "B", "averages")
    legend_pch = c(15, 15, NA)
    legend_color = c("cyan", "pink", "black")
    legend_lty = c(NA, NA, 2)
    legend_lwd = c(NA, NA, 2)
  }
  legend(x = "top", horiz = TRUE,
         legend = legend_text, pch = legend_pch, pt.cex = 1.5,
         col = legend_color, lty = legend_lty, lwd = legend_lwd, bty = "n")
  grid(col = "black")
} 
