setwd("/Users/rafaelfrade/arquivos/desenv/cate_R")
source("cate_r.R")
source("kdensity.R")
library(ggplot2)
library(dplyr)


get_propensity_score = function(d, x) {
  logit = glm(d ~ ., data = x, family = binomial(link = "logit"))
  p_hat = predict(logit, x, type="response")
  return(p_hat)
}

data = read.csv("birthdata.txt", sep = "\t", header = F)

# COVARIATES
fage=data$V2
mage=data$V3
feduc=data$V4
meduc=data$V5
terms=data$V6
gestation=data$V7
prenatal=data$V8
prenatal_visits=data$V9
mom_zip=data$V10
wtgain=data$V11
anemia=data$V12
diabetes=data$V13
hyperpr=data$V14
amnio=data$V15
ultra=data$V16
male=data$V17
feducmiss=data$V18
fagemiss=data$V19
married=data$V20
bweight=data$V21
smoke=data$V22 # TREATMENT
drink=data$V23
kessner_ad=data$V24
kessner_inad=data$V25
med_inc=data$V26
pc_inc=data$V27
long=data$V28
lat=data$V29
popdens=data$V30

data[data$V8==0,]$V8=10
prenatal=data$V8
year_dummies = dummy(data$V1, sep = "_")
year_dummies = as.data.frame(year_dummies)


#MAKE conditioning variable continuous
mage=mage+runif(length(mage))-.5;

y = data$V21 # birth weight
x1 = mage # mother age (conditioning variable)

x_d=data.frame(male, married, drink, diabetes, hyperpr, amnio, ultra, as.integer(terms>0), fagemiss)
x_c=data.frame(mage, meduc, prenatal, prenatal_visits)

x = cbind(as.matrix(x_d), as.matrix(x_c))

#Adding powers, cross products, etc. (constant should NOT be added)
num_rows = dim(x)[1]
num_cols = dim(x)[2]
mage_rep = matrix(rep(mage, num_cols), nrow = num_rows, ncol = num_cols)
cross_products = x*mage_rep
colnames(cross_products) = paste(colnames(cross_products), "cp", sep = "_")

x = cbind(x, cross_products)

# Bandwidth
h = sd(x1)*c(.25, .5, 1, 5)
kerntype='norm'

alpha=0.5

#Treatment
d = smoke
x_df = as.data.frame(x)
conditioning_variable = x_df$mage
x_df$y = y
x_df$smoke = smoke


mean(x_df$y)
x_df = x_df %>% filter(mage > 17) %>% filter(mage < 40)
df_t = x_df %>% filter(smoke == 1)
df_nt = x_df %>% filter(smoke == 0)

df_t %>% filter(y < 1000) %>% count()
df_nt %>% filter(y < 1000) %>% count()

eita = data %>%
  count(V21) %>%
  group_by(V21) %>%          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))


mean(df_t$y)
mean(df_nt$y)

ggplot() +
  geom_smooth(data = df_t, aes(x=mage, y = y, color="treated"), se = T) +
  geom_smooth(data = df_nt,  aes(x=mage, y = y, color="non treated"), se = T)

plot_diff_in_mean = function(df1, df2, )
diff = double()
for (i in 17:37) {
  mean_t = df_t %>% filter(mage > i & mage <= i+1) %>% 
    pull(y) %>% mean()
  mean_nt = df_nt %>% filter(mage > i & mage <= i+1)  %>% 
    pull(y) %>% mean()
  diff[i-16] = mean_t - mean_nt
}
age = 17:37
df_mean = data.frame(diff, age)
ggplot(df_mean, aes(x=age, y=diff)) + geom_line()

