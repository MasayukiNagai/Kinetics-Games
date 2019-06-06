reactKinorder = function(kinorder = c("monomolecular",
                                   "consecutive",
                                   "catalytic",
                                   "bimolecular",
                                   "equilibrium",
                                   "autocatalytic"),
                      a = 139, b = 1, x = 0, cycles = 500, flip = 0){
  
  if(kinorder == "monomolecular"){
    test1 = monomolecular(flip = 0)
    Avalue = test1$a_count
    AAA = log(Avalue)
    plot(x = 1:test1$cycles, y = AAA, type = "l", lwd = 2,
            xlab = "cycles",
            ylab = "ln(counts)", col = "blue")
    
  }
  
  if(kinorder == "consecutive"){
    test1 = consecutive(flip = 0)
    Avalue = test1$a_count
    AAA = log(Avalue)
    plot(x = 1:test1$cycles, y = AAA, type = "l", lwd = 2,
         xlab = "cycles",
         ylab = "ln(counts)", col = "blue")}
  
  if(kinorder == "catalytic"){
    test1 = catalytic(flip = 0)
    Avalue = test1$a_count
    AAA = log10(Avalue)
    plot(x = 1:test1$cycles, y = AAA, type = "l", lwd = 2,
         xlab = "cycles",
         ylab = "counts", col = "blue")}
  
  if(kinorder == "bimolecular"){
    test1 = bimolecular(flip = 0)
    Avalue = test1$a_count
    AAA = 1/Avalue
    plot(x = 1:test1$cycles, y = AAA, type = "l", lwd = 2,
         xlab = "cycles",
         ylab = "counts", col = "blue")}
  
  if(kinorder == "equilibrium"){
    test1 = equilibrium(flip = 0)
    Avalue = test1$a_count
    AAA = log(Avalue)
    plot(x = 1:test1$cycles, y = AAA, type = "l", lwd = 2,
         xlab = "cycles",
         ylab = "counts", col = "blue")}
  
  if(kinorder == "autocatalytic"){
    test1 = autocatalytic(flip = 0)
    Avalue = test1$a_count
    AAA = log(Avalue)
    plot(x = 1:test1$cycles, y = AAA, type = "l", lwd = 2,
         xlab = "cycles",
         ylab = "counts", col = "blue")}
  
}