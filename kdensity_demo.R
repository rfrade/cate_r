setwd("/Users/rafaelfrade/arquivos/desenv/cate_R")
source("kdensity.R")
library(ggplot2)

pop = 1:10
eval_points = -10:20
h = 0.1
fx = kdensity(3, 1:10, 1)
plot_density(1:10, 1:10, 1)

density(pop)
ggplot(as.data.frame(pop), aes(x=pop)) + geom_density()

pop = rnorm(6000, 3, 1)
prob = dnorm(sort(pop), mean = 3, sd = 1)
df_pop = data.frame(x = sort(pop), y = prob)

eval_points = seq(0, 6, 0.1)
fx = kdensity(eval_points, pop, h)
df_est = data.frame(x = eval_points, y = fx)

ggplot() +
  geom_line(data=df_est, aes(x = x, y = y), color="#003f5c") +
  geom_line(data=df_pop, aes(x = x, y = y), color="#ffa600")


plot_density(eval_points, pop, h)


df = data.frame(eval_points = eval_points, pop = pop, fx = fx)
ggplot() + geom_line(df, aes(x = , y = fx))

