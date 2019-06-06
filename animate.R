# convert archive
animate = function(file){
  archive = file$archive
  marbles = file$marbles
  cycles = file$cycles
  board_size = ceiling(sqrt(marbles))
  for (i in 1:cycles){
    board = matrix(data = as.numeric(factor(archive[i, ])), nrow = board_size)
    board_out = lattice::levelplot(board, cuts = 1)
    print(board_out)
    Sys.sleep(0)
  }
}


