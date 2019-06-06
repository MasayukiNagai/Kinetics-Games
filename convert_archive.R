# convert archive
convert_archive = function(file, time = 0){
  archive = file$archive
  marbles = file$marbles
  board_size = ceiling(sqrt(marbles))
  # board = matrix(data = NA, nrow = board_size)
  board = matrix(data = as.numeric(factor(archive[time + 1, ])), nrow = board_size)
  lattice::levelplot(board, cuts = 1)
}
