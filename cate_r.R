
get_grid = function(grid_trim_param = 0.2) {
  lower_bound = quantile(conditioning_variable, grid_trim_param)
  upper_bound = quantile(conditioning_variable, 1-grid_trim_param)
  step_size = (upper_bound-lower_bound)/50
  grid = seq(lower_bound, upper_bound, step_size)
  return(grid)
}

get_propensity_score = function(d, x) {
  logit = glm(d ~ ., data = x, family = binomial(link = "logit"))
  p_hat = predict(logit, x, type="response")
  return(p_hat)
}

trim_data = function(df, alpha=0.5) {
  # TRIM OBS BASED ON ALPHA
  
  if (alpha < 0.5) {
    df$good_obs=as.integer((df$p_hat<=1-alpha) & (df$p_hat>=alpha))
    #print('Percentage of observations dropped=' + (n-sum(good_obs)) /n ))
    df = df[df$good_obs == T,]
    
  } else if (alpha == 0.5) {
    #%if alpha=.5, estimate it using the Crump et al. (2009) method
    alpha = seq(0.01, 0.49, 0.005)
    crit_value = numeric()
    for (j in 1:length(alpha)) {
      
      soma = 2*sum( (df$p_hat*(1-df$p_hat)>=alpha[j]*(1-alpha[j])) / (df$p_hat*(1-df$p_hat)) )
      divid = sum( (df$p_hat*(1-df$p_hat)>=alpha[j]*(1-alpha[j])) )
      
      crit_value[j] = soma / divid
      
    }
    
    alpha=alpha[crit_value>=1/(alpha*(1-alpha))]
    alphahat=min(alpha)
    print(paste('Estimated alpha=',alphahat))
    
    n_before = dim(df)[1]
    df = df[(df$p_hat<=1-alphahat) & (df$p_hat>=alphahat),]
    print(paste('Percentage of observations dropped=',((n_before-dim(df)[1])/(n_before))))
    
  } else {
    return("alpha has to be beteween >0 and 0.5")
  }
  return(df)
}






