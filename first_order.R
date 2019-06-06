first_order = function(file){
  x = file$time
  y = log(file$a_count)
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
  plot(x, y, pch = 19, col = "blue", cex = 0.5, 
       xlab = "time", ylab = "ln(counts of A)", main = main_title)
  fo.lm = lm(y ~ x)
  abline(fo.lm, lwd = 2, lty = 2, col = "blue")
  fo.int = formatC(fo.lm$coefficients[1], digits = 2, format = "f")
  fo.slope = formatC(fo.lm$coefficients[2], digits = 2, format = "e")
  grid(col = "black")
  legend(x = "topright", ncol = 2, legend = c("y-intercept:", "slope:", fo.int, fo.slope), bty = "n")
}
