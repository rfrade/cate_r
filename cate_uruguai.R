library(dummies)

library(ggplot2)

source("cate_r.R")
source("kdensity.R")

setwd("/Users/rafaelfrade/arquivos/desenv/cate_R")
data = read.csv("data_uruguai.csv", sep = ",", header = T)

data = data[data$applied == 1,]

n = dim(data)[1];

cate = function(y, d, x_df, conditioning_variable, h, alpha, grid_trim_param) {
  
  x_df$d = d
  x_df$y = y
  
  grid = get_grid(grid_trim_param)
  
  #STEP 1: Propensity Score
  if (!("p_hat" %in% colnames(x_df))) {
    x_df$p_hat = get_propensity_score(d, x_df)
  }
  
  x_df = trim_data(x_df, alpha)
  n = dim(x_df)[1]
  
  #STEP 2: CATE
  y = x_df$y
  phat = x_df$p_hat
  d = x_df$d
  a=((d*y)/phat - ((1-d)*y)/(1-phat))
  
  #lbd=0.001; %lower bound for f1 density estimate
  f1hat=kdensity(grid, conditioning_variable, h, "norm")
  
  print('Estimating CATE at gridpoints...')
  
  WeightedATEhat=numeric()
  for (j in 1:length(grid)) {
    
    z=(conditioning_variable-grid[j])/h;
    kernel_z = dnorm(z)
    WeightedATEhat[j]=(1/(n*h))*sum( a*kernel_z )
    
  }
  
  #CATE point estimate
  cate_estimate=WeightedATEhat/f1hat #NumX1Grid x 1
  
  cate_df = data.frame(cate_estimate, grid)
  
  return(cate_df)
}

trim_score = 0.01
data = data[data$score_m > -trim_score & data$score_m <= trim_score,]
treat = dim(data[data$score_m > -trim_score & data$score_m <= 0, ])[1]
ntreat = dim(data[data$score_m > 0 & data$score_m <= trim_score,])[1]

p_hat = treat/(treat + ntreat)

y = data$weight
data$age_m = data$age_m+runif(length(data$age_m))-.5;
conditioning_variable = data$age_m
d = data$treated;

data$weight = NULL
data$applied = NULL
data$score_m = NULL
data$p_low_weight = NULL
data$bajo2500 = NULL
data$treated = NULL

#num_rows = dim(x)[1]
#num_cols = dim(x)[2]
#mage_rep = matrix(rep(data$age_m, num_cols), nrow = num_rows, ncol = num_cols)
#cross_products = x*mage_rep
#colnames(cross_products) = paste(colnames(cross_products), "cp", sep = "_")

#x = cbind(x, cross_products)

# Bandwidth
h = sd(data$age_m)*c(.25, .5, 1, 5)
kerntype='norm'

alpha=0.00001

#data$p_hat = get_propensity_score(d, data)
data$p_hat = p_hat

library(dplyr)
#hist(data %>% filter(d==0) %>% pull(p_hat))
#hist(data %>% filter(d==1) %>% pull(p_hat))

graph = ggplot() 
for (h1 in h) {
  cate_df = cate(y, d, data, conditioning_variable, h1, alpha, 0.2)
  graph = graph + geom_line(aes_string(x = "grid", y = "cate_estimate"), data = cate_df)
  print(mean(cate_df$cate_estimate))
}
print(graph)
