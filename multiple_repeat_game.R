multiple_repeat_game = function(reps = 10, 
                       game = c("monomolecular", "bimolecular", 
                                "catalytic", "autocatalytic", 
                                "consecutive", "equilibrium"), 
                       a = 140, b = 0, x = 0, 
                       cycles = 500, coins = 0){
  
  game = match.arg(game, several.ok = TRUE)
  num = length(game)
  
  list_rep_results = list()
  list_a_reps = list()
  list_b_reps = list()
  list_x_reps = list()
  list_a_average = list()
  list_b_average = list()
  list_x_average = list()

  for(i in 1:num){
    game_function = match.fun(game[i])
    rep_results = replicate(reps, game_function(a, b, x, cycles, coins), simplify = FALSE)
    a_reps = as.data.frame(lapply(rep_results, '[', "a_count"))
    b_reps = as.data.frame(lapply(rep_results, '[', "b_count"))
    x_reps = as.data.frame(lapply(rep_results, '[', "x_count"))
    a_average = apply(a_reps, 1, mean)
    b_average = apply(b_reps, 1, mean)
    x_average = apply(x_reps, 1, mean)
    list_rep_results[[i]] = rep_results
    list_a_reps[[i]] = a_reps
    list_b_reps[[i]] = b_reps
    list_x_reps[[i]] = x_reps
    list_a_average[[i]] = a_average
    list_b_average[[i]] = b_average
    list_x_average[[i]] = x_average
    
    # assign(paste("rep_results", i, sep = ""), rep_results)
    # assign(paste("a_reps", i, sep = ""), a_reps)
    # assign(paste("b_reps", i, sep = ""), b_reps)
    # assign(paste("x_reps", i, sep = ""), x_reps)
    # assign(paste("a_average", i, sep = ""), a_average)
    # assign(paste("b_average", i, sep = ""), b_average)
    # assign(paste("x_average", i, sep = ""), x_average)
  }

  out = list("mechanism" = game,
             "repeats" = reps,
             "coins" = rep_results[[1]]$coins,
             "time" = rep_results[[1]]$time,
             "cycles" = rep_results[[1]]$cycles,
             "a_reps" = list_a_reps,
             "b_reps" = list_b_reps,
             "x_reps" = list_x_reps,
             "a_average" = list_a_average,
             "b_average" = list_b_average,
             "x_average" = list_x_average,
             "num" = num)
}
