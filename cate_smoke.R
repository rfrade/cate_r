library(dummies)
library(ggplot2)

source("cate_r.R")
source("kdensity.R")



setwd("/Users/rafaelfrade/arquivos/desenv/cate_R")
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
d = smoke

graph = ggplot() 
i = 1

x_df$p_hat = get_propensity_score(d, x_df)
#x_df$p_hat = read.csv("phat.csv", header = F)$V1

library(dplyr)
#hist(x_df %>% filter(smoke==0) %>% pull(p_hat))
#hist(x_df %>% filter(smoke==1) %>% pull(p_hat))

for (h1 in h) {
  cate_df = cate(y, d, x_df, conditioning_variable, h1, alpha, 0.2)
  graph = graph + geom_line(aes_string(x = "grid", y = "cate_estimate"), data = cate_df)
  print(mean(cate_df$cate_estimate))
}
print(graph)

rm(x, x_c, x_d, year_dummies, mage_rep, data, cross_products)


#cate_df1 = cate(y, d, x_df, conditioning_variable, 26.6422, alpha=0.1, 0.2)

