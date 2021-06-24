library(ggplot2)
library(dplyr)

setwd("/Users/rafaelfrade/arquivos/mestrado/tese/my_code/matlab/")

df = read.csv(file = "data_uruguai.csv")
#df = df %>% filter(age_m > 17) %>% filter(age_m < 40)

df_non_applied = df %>% filter(applied == 0)# %>% filter(educ_m2 == 1)
df_treated = df %>% filter(treated == 1) 
df_non_treated = df %>% filter(treated == 0) %>% filter(applied == 1)

#ggplot(df, aes(x=weight, color=cor, fill=cor)) + geom_histogram(binwidth=30)

ggplot() +
  geom_smooth(data = df_non_applied, aes(x=age_m, y = weight, color="didnt apply"), se = T) +
  geom_smooth(data = df_treated, aes(x=age_m, y = weight, color="treated"), se = F) +
  geom_smooth(data = df_non_treated, aes(x=age_m, y = weight, color="non treated"), se = F)

ggplot() +
  geom_smooth(data = df_treated, aes(x=p_hat, y = weight, color="treated"), se = F) +
  geom_smooth(data = df_non_treated,  aes(x=p_hat, y = weight, color="non treated"), se = F)


df_treated = df_treated %>% filter(score_m > -0.05)
df_non_treated = df_non_treated %>% filter(score_m < 0.05)
ggplot() +
  geom_smooth(data = df_treated, method = "loess", aes(x=age_m, y = weight, color="treated"), se = F) +
  geom_smooth(data = df_non_treated, method = "loess", aes(x=age_m, y = weight, color="non treated"), se = F)


ggplot() +
  geom_smooth(data = df_treated, method = "loess", aes(x=age_m, y = p_low_weight, color="treated"), se = F) +
  geom_smooth(data = df_non_treated, method = "loess", aes(x=age_m, y = p_low_weight, color="non treated"), se = F)

hist(df_treated$p_hat)
hist(df_non_treated$p_hat)
