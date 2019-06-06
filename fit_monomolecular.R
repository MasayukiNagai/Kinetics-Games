fit_monomolecular = function(file, a_initial, a_end, rate_const){
  
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
  
  x = file$time
  y = file$a_count
  cycles = file$cycle
  y_nls = nls(y ~ a0 * exp(-k * x) + at, 
              start = list(a0 = a_initial, 
                           at = a_end, 
                           k = rate_const))
  model_coef = coef(y_nls)
  plot(x, y, pch = 19, cex = 0.25, col = "blue", xlab = "time", 
       ylab = "counts", main = main_title)
  grid(col = "black")
  lines(x, y = y_nls$m$fitted(), lty = 1, lwd = 2, col = "blue")
  legend(x = "topright", ncol = 2, legend = c("a_initial:", "a_end:", "k:", formatC(model_coef[1], digits = 2, format = "f"), formatC(model_coef[2], digits = 2, format = "f"), formatC(model_coef[3], digits = 2, format = "e")), bty = "n")

}



