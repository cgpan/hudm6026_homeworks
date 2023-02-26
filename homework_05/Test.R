# write the original function
f_origin <- function(x_1, x_2){
  out_ <- x_1^4+x_2^4-2*x_1^2+2*x_1*x_2-3*x_2^2+6*x_1-4*x_2+10
  return(out_)
}

# write the two partial derivatives
dfdx_1 <- function(x_1, x_2){
  return(4*x_1^3-4*x_1 + 2*x_2+6)
}
dfdx_2 <- function(x_1, x_2){
  return(4*x_2^3+2*x_1-6*x_2-4)
}

# write the gradient descent function
grad_f <- function(x_1, x_2){c(dfdx_1(x_1,x_2), dfdx_2(x_1,x_2))}
gradient_ <- grad_f(1,2)
gradient_norm <- gradient_/sqrt(gradient_%*%gradient_)
gradient_norm
p_new <- c(1,2)-0.02*gradient_norm
p_new
f_origin(1,2)
f_origin(p_new[1], p_new[2])

gradient_d <- function(f_original, # the original function
                       grad_f, # the gradient function
                       start_point, # the start point, a vector
                       max_iter=100, # the maximum number of iteration
                       alpha=0.03, # learning rate/ step size
                       epsilon=1e-5 # stopping criterion
){
  # initial settings
  p_old <- start_point;i <- 1;check <- 1
  while (check > epsilon) {
    # print the iteration information at each 10 rounds
    if (i ==1 | i%%10 == 0){
      print(paste0("Iter ", i, "; f(x_1, x_2)= ", f_original(p_old[1], p_old[2])))
    }
    # Stop condition and warning
    if (i > max_iter) {
      print("Exceed maximum number of iterations")
      break
    }
    if (abs(p_old[1]) > 3 |abs(p_old[2] >3)) {
      print("Exceed the Given Range")
      break
    }
    # load the gradient
    gradient_ <- grad_f(p_old[1], p_old[2])
    # normalize the the gradient vector
    gradient_norm <- gradient_/sqrt(gradient_%*%gradient_)
    # update the point coordination with gradient * learning rate
    p_new <- p_old - alpha*gradient_norm
    # check the updating rate of p_new, if less than epsilong, stop
    check <- sqrt((p_new-p_old)%*%(p_new-p_old))/ sqrt(p_old %*% p_old)
    # redefine the old point to send for next updating 
    p_old <- p_new
    i <- i + 1
  }
  print(paste0("The minimum point is around ", 
               round(p_old[1],4)," ",
               round(p_old[2],4)," ",
               round(f_origin(p_old[1],p_old[2]),4)))
}

gradient_d(f_origin, grad_f, c(1,2))

20%%10