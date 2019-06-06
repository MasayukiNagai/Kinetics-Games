# temp storage from Khoa Vu scripts

# from monomolecular.r
Avalue = test1$a_initial
test1 = monomolecular(flip = 3)

FLIP = test1$flip
t = 1:test1$cycles
AAA =  Avalue*exp(-t/(Avalue*(2^FLIP)))

plot(x = 1:test1$cycles, y = test1$a_count, type = "l", lwd = 2,
      ylim = c(0, test1$particles), xlab = "cycles",
      ylab = "counts", col = "blue")

lines(x = 1:test1$cycles, y = AAA, lty = 2, lwd = 2,
       ylim = c(0, test1$particles), xlab = "cycles",
       ylab = "counts", col = "green")

# from equilibrium
test1 = equilibrium(a = 1000, cycles = 5000, flip = 0)
Avalue = test1$a_initial
Bvalue = test1$b_initial
Particles = test1$particles

FLIP = test1$flip
t = 1:test1$cycles
k1 = 1/Particles
k2 = 1/(2^(length(FLIP)/2) * Particles)
AAA = Avalue*(1/(k1+k2))*(k2+k1*exp(-(k1+k2)*t))+Bvalue*(k2/(k1+k2))*(1-exp(-(k1+k2)*t))

plot(x = 1:test1$cycles, y = test1$a_count, type = "l", lwd = 2,
     ylim = c(0, test1$particles), xlab = "cycles",
     ylab = "counts", col = "blue")

lines(x = 1:test1$cycles, y = AAA, lty = 2, lwd = 2,
      ylim = c(0, test1$particles), xlab = "cycles",
      ylab = "counts", col = "green")

# # lines(x = 1:test1$cycles, y = test1$b_count, lty = 2, lwd = 2,
# #       ylim = c(0, test1$particles), xlab = "cycles",
# #       ylab = "counts", col = "green")

#  test1 = consecutive(flip = 0)
#  Avalue = test1$a_initial
#  Particles = test1$particles
#  Xvalue = test1$x_initial
#  
#  FLIP = test1$flip
#  t = 1:test1$cycles
#  k = (1/Particles)*(1/2^FLIP)
#  AAA =  Avalue*exp(-t*k) 
#  
#  plot(x = 1:test1$cycles, y = test1$a_count, type = "l", lwd = 2,
#       ylim = c(0, test1$particles), xlab = "cycles",
#       ylab = "counts", col = "blue")
#  
#  lines(x = 1:test1$cycles, y = AAA, lty = 2, lwd = 2,
#        ylim = c(0, test1$particles), xlab = "cycles",
#        ylab = "counts", col = "green")
#  
#  
# # lines(x = 1:test1$cycles, y = test1$b_count, lty = 2, lwd = 2,
# #       ylim = c(0, test1$particles), xlab = "cycles",
# #       ylab = "counts", col = "green")
# # lines(x = 1:test1$cycles, y = test1$x_count, lty = 3, lwd = 2,
# #       ylim = c(0, test1$particles), xlab = "cycles",
# #       ylab = "counts", col = "red")

# from bimolecular


# test1 = bimolecular(flip = 0)
# Avalue = test1$a_initial
# FLIP = test1$flip
# t = 1:test1$cycles
# Particles = test1$particles
# AAA =  Avalue/(1+(Avalue*t*(2*(1/Particles)*(1/(Particles-1)))))
# 
# 
#  
#  plot(x = 1:test1$cycles, y = test1$a_count, type = "l", lwd = 2,
#       ylim = c(0, test1$particles), xlab = "cycles",
#       ylab = "counts", col = "blue")
#  lines(x = 1:test1$cycles, y = AAA, lwd = 2, lty = 2,
#        ylim = c(0, test1$particles), xlab = "cycles",
#        ylab = "counts", col = "green")
# 
#  

# from autocatalytic


# test1 = autocatalytic(flip = 0)
# Avalue = test1$a_initial
# Bvalue = test1$b_initial
# 
# Particles = test1$particles
# 
# FLIP = test1$flip
# t = 1:test1$cycles
# k = 2 * (1/Particles) * (Bvalue/(Particles))/(2^FLIP)
# AAA = (Avalue + Bvalue) + (Avalue + Bvalue)/(1 + (Avalue/Bvalue)*exp(-k*t*(Avalue + Bvalue))) 
# 
# 
# plot(x = 1:test1$cycles, y = test1$a_count, type = "l", lwd = 2,
#      ylim = c(0, test1$particles), xlab = "cycles",
#      ylab = "counts", col = "blue")
# 
# lines(x = 1:test1$cycles, y = AAA, lty = 2, lwd = 2,
#       ylim = c(0, test1$particles), xlab = "cycles",
#       ylab = "counts", col = "green")
# 
# # lines(x = 1:test1$cycles, y = test1$b_count, lty = 2, lwd = 2,
# #       ylim = c(0, test1$particles), xlab = "cycles",
# #       ylab = "counts", col = "green")

# from catalytics
# test1 = catalytic(flip = 0)
# Avalue = ln(test1$a_initial)
# Xvalue = test1$x_initial
# 
# Particles = test1$particles
# 
# FLIP = test1$flip
# t = 1:test1$cycles
# k = 2 * (1/Particles) * (Xvalue/(Particles))/(2^FLIP)
# AAA =  Avalue*exp(-k*t)
#  
#  
#  plot(x = 1:test1$cycles, y = test1$a_count, type = "l", lwd = 2,
#       ylim = c(0, test1$particles), xlab = "cycles",
#       ylab = "counts", col = "blue")
#  
#  lines(x = 1:test1$cycles, y = AAA, lty = 2, lwd = 2,
#        ylim = c(0, test1$particles), xlab = "cycles",
#        ylab = "counts", col = "green")
#  
#  # lines(x = 1:test1$cycles, y = test1$b_count, lty = 2, lwd = 2,
#  #       ylim = c(0, test1$particles), xlab = "cycles",
#  #       ylab = "counts", col = "green")
#  # lines(x = 1:test1$cycles, y = test1$x_count, lty = 3, lwd = 2,
#  #       ylim = c(0, test1$particles), xlab = "cycles",
#  #       ylab = "counts", col = "red")
