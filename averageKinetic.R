averageKin = function(replications = 5,
                      kinmodel = c("monomolecular",
                                 "consecutive",
                                 "catalytic",
                                 "bimolecular",
                                 "equilibrium"),
                      a = 140, b = 0, x = 0, cycles = 500, flip = 0){
  
  kinmodel = match.arg(kinmodel)
  if(kinmodel == "monomolecular"){
    out = replicate(replications, monomolecular(a, b, x, cycles, flip), 
                    simplify = FALSE)
  } 
  
  test4 = averageKin(replications = 100)
  
  kinmodel = test4[[1]]$mechanism
  cycles = test4[[1]]$cycles
  replications = length(test4)
  
  plot(x = 1:cycles, y = test4[[1]]$a_count, type = "l",
       xlab = "cycles", ylab = "number of A", col = "cyan",
       ylim = c(0, test4[[1]]$particles),
       main = paste0("mechanism: ", kinmodel, "; replications: ",
       replications),
  grid(col = "gray"))
  
  for (i in 1:length(test4)){
    lines(x = 1:cycles, y = test4[[1]]$a_count, col = "cyan")
    lines(x = 1:cycles, y = test4[[1]]$b_count, col = "pink")
  }
  
  a_reps = as.data.frame(lapply(test4, '[', "a_count"))
  b_reps = as.data.frame(lapply(test4, '[', "b_count"))
  a_average = apply(a_reps, 1, mean)
  b_average = apply(b_reps, 1, mean)
  lines(x = 1:cycles, y = a_average, lty = 1, col = "darkblue", lwd = 2)
  lines(x = 1:cycles, y = b_average, lty = 1, col = "red", lwd = 2)
  legend(x = "right", legend = c("A: individual trials", "A: average",
                                 "B: individual trials", "B: average"),
         col = c("cyan", "darkblue", "pink", "red"),
         text.col = c("cyan", "darkblue", "pink", "red"),
         lwd = 2, lty = "n")
  
  
}
          