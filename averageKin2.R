averageKin = function(replications = 5,
                      kinmodel = c("monomolecular",
                                   "consecutive",
                                   "catalytic",
                                   "bimolecular",
                                   "equilibrium",
                                   "autocatalytic"),
                      a = 139, b = 1, x = 0, cycles = 500, coins = 0){
  
  kinmodel = match.arg(kinmodel)
  
  if(kinmodel == "monomolecular"){
    df = replicate(replications, monomolecular(a, b, x, cycles, coins), 
                    simplify = FALSE)
    cycles = df[[1]]$cycles
    a_reps = as.data.frame(lapply(df, '[', "a_count"))
    b_reps = as.data.frame(lapply(df, '[', "b_count"))
    a_average = apply(a_reps, 1, mean)
    b_average = apply(b_reps, 1, mean)
    matplot(x = 1:(cycles+1), y = a_reps, type = "l",
         xlab = "cycles", ylab = "number of A", col = "cyan",
         ylim = c(0, df[[1]]$particles),
         main = paste0("mechanism: ", kinmodel, "; replications: ",
                       replications))
         grid(col = "gray")
    matlines(x = 1:cycles, y = b_reps, col = "pink")
    lines(x = 1:cycles, y = a_average, lty = 1, col = "darkblue", lwd = 2)
    lines(x = 1:cycles, y = b_average, lty = 1, col = "red", lwd = 2)
    legend(x = "right", legend = c("A: individual trials", "A: average",
                                   "B: individual trials", "B: average"),
           col = c("cyan", "darkblue", "pink", "red"),
           text.col = c("cyan", "darkblue", "pink", "red"),
           lwd = 2, bty = "n")
  } 
  if(kinmodel == "equilibrium"){
    df = replicate(replications, equilibrium(a, b, x, cycles, flip), 
                   simplify = FALSE)
    cycles = df[[1]]$cycles
    a_reps = as.data.frame(lapply(df, '[', "a_count"))
    b_reps = as.data.frame(lapply(df, '[', "b_count"))
    a_average = apply(a_reps, 1, mean)
    b_average = apply(b_reps, 1, mean)
    matplot(x = 1:cycles, y = a_reps, type = "l",
            xlab = "cycles", ylab = "number of A", col = "cyan",
            ylim = c(0, df[[1]]$particles),
            main = paste0("mechanism: ", kinmodel, "; replications: ",
                          replications))
    grid(col = "gray")
    matlines(x = 1:cycles, y = b_reps, col = "pink")
    lines(x = 1:cycles, y = a_average, lty = 1, col = "darkblue", lwd = 2)
    lines(x = 1:cycles, y = b_average, lty = 1, col = "red", lwd = 2)
    legend(x = "right", legend = c("A: individual trials", "A: average",
                                   "B: individual trials", "B: average"),
           col = c("cyan", "darkblue", "pink", "red"),
           text.col = c("cyan", "darkblue", "pink", "red"),
           lwd = 2, bty = "n")
  } 
  if(kinmodel == "consecutive"){
    df = replicate(replications, consecutive(a, b, x, cycles, flip), 
                   simplify = FALSE)
    cycles = df[[1]]$cycles
    a_reps = as.data.frame(lapply(df, '[', "a_count"))
    b_reps = as.data.frame(lapply(df, '[', "b_count"))
    a_average = apply(a_reps, 1, mean)
    b_average = apply(b_reps, 1, mean)
    matplot(x = 1:cycles, y = a_reps, type = "l",
            xlab = "cycles", ylab = "number of A", col = "cyan",
            ylim = c(0, df[[1]]$particles),
            main = paste0("mechanism: ", kinmodel, "; replications: ",
                          replications))
    grid(col = "gray")
    matlines(x = 1:cycles, y = b_reps, col = "pink")
    lines(x = 1:cycles, y = a_average, lty = 1, col = "darkblue", lwd = 2)
    lines(x = 1:cycles, y = b_average, lty = 1, col = "red", lwd = 2)
    legend(x = "right", legend = c("A: individual trials", "A: average",
                                   "B: individual trials", "B: average"),
           col = c("cyan", "darkblue", "pink", "red"),
           text.col = c("cyan", "darkblue", "pink", "red"),
           lwd = 2, bty = "n")
  }
  if(kinmodel == "catalytic"){
    df = replicate(replications, catalytic(a, b, x, cycles, flip), 
                   simplify = FALSE)
    cycles = df[[1]]$cycles
    a_reps = as.data.frame(lapply(df, '[', "a_count"))
    b_reps = as.data.frame(lapply(df, '[', "b_count"))
    a_average = apply(a_reps, 1, mean)
    b_average = apply(b_reps, 1, mean)
    matplot(x = 1:cycles, y = a_reps, type = "l",
            xlab = "cycles", ylab = "number of A", col = "cyan",
            ylim = c(0, df[[1]]$particles),
            main = paste0("mechanism: ", kinmodel, "; replications: ",
                          replications))
    grid(col = "gray")
    matlines(x = 1:cycles, y = b_reps, col = "pink")
    lines(x = 1:cycles, y = a_average, lty = 1, col = "darkblue", lwd = 2)
    lines(x = 1:cycles, y = b_average, lty = 1, col = "red", lwd = 2)
    legend(x = "right", legend = c("A: individual trials", "A: average",
                                   "B: individual trials", "B: average"),
           col = c("cyan", "darkblue", "pink", "red"),
           text.col = c("cyan", "darkblue", "pink", "red"),
           lwd = 2, bty = "n")
  }
  if(kinmodel == "bimolecular"){
    df = replicate(replications, bimolecular(a, b, x, cycles, flip), 
                   simplify = FALSE)
    cycles = df[[1]]$cycles
    a_reps = as.data.frame(lapply(df, '[', "a_count"))
    b_reps = as.data.frame(lapply(df, '[', "b_count"))
    a_average = apply(a_reps, 1, mean)
    b_average = apply(b_reps, 1, mean)
    matplot(x = 1:cycles, y = a_reps, type = "l",
            xlab = "cycles", ylab = "number of A", col = "cyan",
            ylim = c(0, df[[1]]$particles),
            main = paste0("mechanism: ", kinmodel, "; replications: ",
                          replications))
    grid(col = "gray")
    matlines(x = 1:cycles, y = b_reps, col = "pink")
    lines(x = 1:cycles, y = a_average, lty = 1, col = "darkblue", lwd = 2)
    lines(x = 1:cycles, y = b_average, lty = 1, col = "red", lwd = 2)
    legend(x = "right", legend = c("A: individual trials", "A: average",
                                   "B: individual trials", "B: average"),
           col = c("cyan", "darkblue", "pink", "red"),
           text.col = c("cyan", "darkblue", "pink", "red"),
           lwd = 2, bty = "n")
  } 
  if(kinmodel == "autocatalytic"){
    df = replicate(replications, autocatalytic(a, b, x, cycles, flip), 
                   simplify = FALSE)
    cycles = df[[1]]$cycles
    a_reps = as.data.frame(lapply(df, '[', "a_count"))
    b_reps = as.data.frame(lapply(df, '[', "b_count"))
    a_average = apply(a_reps, 1, mean)
    b_average = apply(b_reps, 1, mean)
    matplot(x = 1:cycles, y = a_reps, type = "l",
            xlab = "cycles", ylab = "number of A", col = "cyan",
            ylim = c(0, df[[1]]$particles),
            main = paste0("mechanism: ", kinmodel, "; replications: ",
                          replications))
    grid(col = "gray")
    matlines(x = 1:cycles, y = b_reps, col = "pink")
    lines(x = 1:cycles, y = a_average, lty = 1, col = "darkblue", lwd = 2)
    lines(x = 1:cycles, y = b_average, lty = 1, col = "red", lwd = 2)
    legend(x = "right", legend = c("A: individual trials", "A: average",
                                   "B: individual trials", "B: average"),
           col = c("cyan", "darkblue", "pink", "red"),
           text.col = c("cyan", "darkblue", "pink", "red"),
           lwd = 2, bty = "n")
  } 
}

