test1 = consecutive()

time = 1:test1$cycles
y = test1$a_count

plot(time, y, type = "l", lty = 3)

y_nls = nls(y ~ a_initial * exp(-k*time) + a_end, 
            start = list(a_initial = test1$a_initial, 
                         k = (1/Particles)
                         , a_end = 0))

summary(y_nls)

lines(x = time, y = y_nls$m$fitted(), lty = 1, col = "blue")