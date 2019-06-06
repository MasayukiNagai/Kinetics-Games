catalytic = function(a = 70, b = 0, x = 40, cycles = 500, probability = 100){
  
  # total marbles in mixture
  marbles = a + b + x
  
  # create vector of times
  time = seq(0,cycles)
  
  # create vectors with id numbers for marbles and probability
  id_marble = seq(1:marbles)
  id_prob = seq(1:100)
  
  # create vectors to store counts of a, b, and x over time
  a_count = rep(a, cycles + 1)
  b_count = rep(b, cycles + 1)
  x_count = rep(x, cycles + 1)
  
  # create vector giving composition of marble mixture in jar
  jar = c(rep("A", a), rep("B", b), rep("X", x))
  
  # draw marble; pick number for probability
  # evalute and change A to B or leave as is
  # update counts for A, B, and X
  for (i in 2:(cycles + 1)){
    draw_marble = sample(id_marble, 2)
    pick_prob = sample(id_prob, 1)
    if(jar[draw_marble[1]] == "A" & jar[draw_marble[2]] == "X" 
       & pick_prob <= probability){
      jar[draw_marble[1]] = "B" 
    } else {
      if(jar[draw_marble[1]] == "X" & jar[draw_marble[2]] == "A" 
           & pick_prob <= probability){
      jar[draw_marble[2]] = "B" 
      }
      }
    a_count[i] = length(which(jar == "A"))
    b_count[i] = length(which(jar == "B"))
    x_count[i] = length(which(jar == "X"))
  }
  
  # return results of simulation
  out = list("mechanism" = "catalytic",
             "probability" = probability,
             "marbles" = marbles,
             "time" = time,
             "a_count" = a_count,
             "b_count" = b_count,
             "x_count" = x_count,
             "cycles" = cycles)
}
