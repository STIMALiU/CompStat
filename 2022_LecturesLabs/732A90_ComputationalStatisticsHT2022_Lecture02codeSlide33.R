## "wild" function , global minimum at about -15.81515
fw <- function (x)
         10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80
plot(fw, -50, 50, n = 1000, main = "optim() minimising 'wild function'")
# method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")     
res <- optim(50, fw, method = "SANN",lower = -Inf, upper = Inf, control = list(maxit = 20000, temp = 20, parscale = 20), hessian = FALSE)
# res$par= -15.8144, res$value=67.47249, res$$counts["function"]=20000,res$$counts["gradient"]=NA, res$convergence=0 (did!), res$message=NULL



