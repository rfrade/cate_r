
plot_density = function (eval_points, pop, h, kernel_type = "norm") {
  fx = kdensity(eval_points, pop, h, kernel_type)
  ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(x = eval_points, y = fx))
}

kdensity = function(eval_points, pop, h, kernel_type) {
  fx = double()
  i = 1
  for (x in eval_points) {
    fx[i] = kernel(x, pop, h, kernel_type = "norm")
    i = i + 1
  }
  return(fx)
}

kernel = function(x, pop, h, kernel_type = "norm") {
  n = length(pop)
  z = (pop-x) / h
  kernel_values = c()
  if (kernel_type == "norm") {
    kernel_values = dnorm(z)
  } else {
    kernel_values = epa_kernel(z)
  }
  result = (1/(n*h))*sum(kernel_values)
  return(result)
}

epa_kernel = function(z) {0.75*(1-z^2)*(z>-1)*(z<1)}




