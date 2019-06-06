play_game = function(game = c("monomolecular", "bimolecular",
                             "catalytic", "autocatalytic",
                             "consecutive", "equilibrium"), a = 140, b = 0, x = 0, 
                    cycles = 500, coins = 0){
  
  game = match.arg(game)
  game_function = match.fun(game)
  game_result = game_function(a, b, x, cycles, coins)
}
  
