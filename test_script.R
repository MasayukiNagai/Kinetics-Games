# set working directory before running this code

# run these lines to source files to gain access to functions
source("monomolecular.R")
source("simpleplot.R")
source("repeat_game.R")
source("rep_plot.R")

# run these line to complete and a plot a single monomolecular simulation
mono_test = monomolecular(coins = 1)
simpleplot(file = mono_test)

# run these lines to complete and plot mutliple monomolecular simulations
mono_repeat = repeat_game(game = "monomolecular", coins = 1)
rep_plot(mono_repeat)
