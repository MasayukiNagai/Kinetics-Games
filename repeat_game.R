repeat_game = function(reps = 10, 
                       game = c("monomolecular", "bimolecular", 
                                "catalytic", "autocatalytic", 
                                "consecutive", "equilibrium"), 
                       a = 140, b = 0, x = 0, 
                       cycles = 500, probability = 0){
  game = match.arg(game)
  game_function = match.fun(game)
  rep_results = replicate(reps, game_function(a, b, x, cycles, probability), simplify = FALSE)
  a_reps = as.data.frame(lapply(rep_results, '[', "a_count"))
  b_reps = as.data.frame(lapply(rep_results, '[', "b_count"))
  x_reps = as.data.frame(lapply(rep_results, '[', "x_count"))
  a_average = apply(a_reps, 1, mean)
  b_average = apply(b_reps, 1, mean)
  x_average = apply(x_reps, 1, mean)
  out = list("mechanism" = game,
             "repeats" = reps,
             "coins" = rep_results[[1]]$coins,
             "time" = rep_results[[1]]$time,
             "cycles" = rep_results[[1]]$cycles,
             "a_reps" = a_reps,
             "b_reps" = b_reps,
             "x_reps" = x_reps,
             "a_average" = a_average,
             "b_average" = b_average,
             "x_average" = x_average)
}
