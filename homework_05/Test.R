golden <- function(f, int, precision = 1e-6)
{
  rho <- (3-sqrt(5))/2 # ::: Golden ratio
  # ::: Work out first iteration here
  f_a <- f(int[1] + rho*(diff(int)))
  f_b <- f(int[2] - rho*(diff(int)))
  ### How many iterations will we need to reach the desired precision?
  N <- ceiling(log(precision/(diff(int)))/log(1-rho))
  for (i in 1:(N))                    # index the number of iterations
  {
    if (f_a < f_b)  
    {
      int[2] <- int[2] - rho*(diff(int))
      f_b <- f(int[2])
    } else{
      if (f_a >= f_b)
      {
        int[1] <- int[1] + rho*(diff(int))
        f_a <- f(int[1])
      } }
  }
  int
}
golden(f_neg,c(0,4))
(3-sqrt(5))/2