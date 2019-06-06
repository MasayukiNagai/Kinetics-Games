simulateKG = function(game = "monomolecular", a = 150, b = 0, x = 0, cycles = 500, coins = 0){
  
  source("monomolecular.R")
  source("bimolecular.R")
  source("catalytic.R")
  source("autocatalytic.R")
  source("consecutive.R")
  source("equilibrium.R")
  
  if(game == "monomolecular")
    out = monomolecular(a, b, x, cycles, coins)
  else if(game == "bimolecular")
    out = bimolecular(a, b, x, cycles, coins)
  else if(game == "catalytic")
    out = catalytic(a, b, x, cycles, coins)
  else if(game == "autocatalytic")
    out = autocatalytic(a, b, x, cycles, coins)
  else if(game == "consecutive")
    out = consecutive(a, b, x, cycles, coins)
  else if(game == "equilibrium")
    out = equilibrium(a, b, x, cycles, coins)
  else 
    stop("selected game does not exist")
  
  output = list("mechanism" = out$mechanism,
                "cycles"= out$cycles,
                "coins" = out$coins,
                "repeats" = repeats,
                "a_count" = out$a_count,
                "b_count" = out$b_count,
                "x_count" = out$x_count,
                "time" = out$time,
                "marbles" = out$marbles
    )
  invisible(output)
  
}