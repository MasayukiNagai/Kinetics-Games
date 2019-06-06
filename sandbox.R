mono_rep = repeat_game(reps = 25, game = "monomolecular", a = 500, cycles = 1000)

file = mono_rep
a_initial = y$a_count[1]
a_end = 0
rate_const = 1/y$a_count[1]

x = file$time
y = file$a_rep
reps = file$repeats

initial_A = rep(y$a_count[1], reps)
end_A = rep(0, reps)
constant_k = rep(1/y$a_count[1], reps)


for (i in 1:reps){
  y_nls = nls(y[,i] ~ a0 * exp(-k * x) + at, 
              start = list(a0 = a_initial, 
                           at = a_end, 
                           k = rate_const))
  model_coef = coef(y_nls)
  initial_A[i] = model_coef[1]
  end_A[i] = model_coef[2]
  constant_k[i] = model_coef[3]
}

model_output = data.frame(initial_A, end_A, constant_k)
model_output
apply(model_output,2, mean)
apply(model_output,2, sd)

old.par = par(mfrow = c(1, 3))
# boxplot(model_output[1])
stripchart(model_output[1], vertical = TRUE, method = "jitter", pch = 19, col = "blue")
# boxplot(model_output[2])
stripchart(model_output[2], vertical = TRUE, method = "jitter", pch = 19, col = "blue")
# boxplot(model_output[3])
stripchart(model_output[3], vertical = TRUE, method = "jitter", pch = 19, col = "blue")
par(old.par)
