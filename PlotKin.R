  sim1 = monomolecular(flip = 0)
  sim2 = monomolecular(flip = 1)
  sim3 = monomolecular(flip = 2)
  sim4 = monomolecular(flip = 3)
  sim5 = monomolecular(flip = 4)
  sim6 = monomolecular(flip = 5)
  plotSim = function(filenames = list(sim1, sim2, sim3, sim4, sim5, sim6), legend_text = NULL, 
                     legend_position = c("topleft", "topright", "bottomleft", 
                                         "bottomright"), main_title = "Plot Kinetic",
                                       line_widths = c(2, 2, 2, 2, 2, 2), 
                                      line_types = c(1, 2, 3, 4, 5, 6), 
                                       point_symbols = c(21, 22, 23, 24, 25, 26), 
                              line_colors = c("blue", "red", "purple",
                                              "green", "black", "gray"))
  { numfiles = length(filenames)
  xmin = rep(0, numfiles)
  xmax = rep(0, numfiles)
  ymin = rep(0, numfiles)
  ymax = rep(0, numfiles)
  for (i in 1:numfiles) {
    xmin[i] = 1
    xmax[i] = max(filenames[[i]]$cycles)
    ymin[i] = min(filenames[[i]]$a_count)
    ymax[i] = max(filenames[[i]]$a_count)
  }
  xmin_id = which.min(xmin)
  xmax_id = which.max(xmax)
  ymin_id = which.min(ymin)
  ymax_id = which.max(ymax)

   plot(x = 1:filenames[[1]]$cycles, y = filenames[[1]]$a_count, 
        xlim = c(xmin[xmin_id],xmax[xmax_id]), ylim = c(ymin[ymin_id], 
      ymax[ymax_id]*1.4), type = "l", lwd = line_widths[1], lty = line_types[1], 
      col = line_colors[1], main = main_title, xlab = "cycles", ylab = "counts")
   
   for (t in 2:numfiles){
   lines(x = 1:filenames[[t]]$cycles, y = filenames[[t]]$a_count, type = "l", 
         lwd = line_widths[t], lty = line_types[t], col = line_colors[t])
   }
   
   if (is.null(legend_text) == TRUE) {
     legend(x = "topright", legend = c("monomolecular no flip","monomolecular 1 flip","monomolecular 2 flip","monomolecular 3 flip",
                                            "monomolecular 4 flip", "monomolecular 5 flip"), lwd = 2, 
            lty = 2, col = c("blue", "red", "purple","green", "black", "gray"), bty = "n")
   }
  }
  