quantplot<-function(formula=y~x,data,tau=c(0.1,0.25,0.5,0.75,0.9)){
  xyplot(y~x , data =data, 
         type = c("g"),
         auto.key=list(x=.8,y=.35,cex=.8,cex.title=.8, title="", points=TRUE), 
         scales=list(tck=-1),ylab=list("y",font=3),
         xlab=list("s",font=3),
         panel=function(x,y,...){
           panel.xyplot(x,y)
           panel.grid()
           panel.abline(rq(y ~ x, tau = 0.5))
           panel.points(x, y, cex = 0.5, col = "gray")
           panel.abline(rq(y ~ x, tau = 0.5), col = "blue")
           panel.abline(lm(y ~ x), lty = 2, col = "red")
           taus <- tau[tau!=0.5]
           for (i in 1:length(taus)) {
             panel.abline(rq(y ~ x, tau = taus[i]),
                          col = "orange")
           }
           
         }
  )
}

add_var <- function(dat, value, name) {
  dat[name] <- value + if(exists(name, dat)) dat[[name]] else 0
  dat
}

###########################
benchmark_mean_variab_global <- function(thatb, T, h, Phi, w, W=diag(w), gamma, Omega, ...) {
  stopifnot(require(alabama))
  n <- length(thatb)
  stopifnot(n==length(w))
  stopifnot(nrow(Phi)==n,nrow(Phi)==ncol(Phi))
  stopifnot(h>=0)
  # Define some utility functions for parts of the optimization problem
  squared_error <- function(delta) { t(delta-thatb) %*% Phi %*% (delta-thatb) }
  variation <- function(delta)  { t(delta) %*% Omega %*% delta }
  objective <- function(delta) { squared_error(delta) + gamma*variation(delta) }
  if (!is.null(T)) {
    mean_violation <- function(delta) { as.vector(w %*% (delta - T)) }
  } else {
    mean_violation <- function(delta) { NA }
  }
  if (!is.null(h)) {
    var_violation <- function(delta) { as.vector(t(delta - T) %*% W %*% (delta - T) - h) }
  } else {
    var_violation <- function(delta) { NA }
  }
  # Define a vector-valued function for the equality constraints, as required by
  # auglag()
  # If there are non-null target values for both constraints
  if (!is.null(T) && !is.null(h)) {
    eqconstraints <- function(delta) {
      c(mean_violation(delta), var_violation(delta))
    }
  } else if (!is.null(T)) {
    # Constrain means only
    eqconstraints <- function(delta) { mean_violation(delta) }
  } else if (!is.null(h)) {
    # Constrain variabilities only --- odd but not impossible
    eqconstraints <- function(delta) { var_violation(delta) }
  }
  # Start the optimization from the Bayes estimates (which generally do not
  # satisfy the constraints); turn off auglag's habit of printing progress
  # messagesa as it goes
  fit <- auglag(c(thatb), fn = objective, heq=eqconstraints,
                control.outer=list(trace=FALSE),...)
  delta <- fit$par
  lambda <- fit$lambda
  return(list(delta=delta, convergence=fit$convergence, lambda=lambda,
              mean_violation=mean_violation(delta), var_violation=var_violation(delta),
              smoothness=variation(delta)))
}

benchmark_mean <- function(thatb, T, Phi, w, gamma, Omega) {
  sig <- Phi + gamma * Omega
  if (det(sig)>0) {
    siginv <- solve(sig)
    delta.hat <- siginv%*%(Phi %*% thatb + t(as.numeric(solve(t(w) %*% siginv %*% w))*(T - t(w) %*% siginv %*% Phi %*% thatb)%*%w))
  } else {
    fit <- benchmark_mean_variab_global(thatb=thatb, T=T, h=NULL, Phi=Phi, w=w,
                                        W=matrix(0,nrow=length(w),ncol=length(w)), gamma=gamma, Omega=Omega)
    delta.hat <- fit$delta
  }
  return(delta.hat)
}

# Cross-validate the bench-marking functions
# smoothing factor gamma is constrained to be >= 0
# Inputs:
# initial guess at smoothing factor (gamma.0)
# matrix of loss weights (Phi),
# vector of Bayes estimates (thatb),
# the function which does benchmarking (bencher)
# flag for returning gamma only (gamma.only)
# other arguments to that function (...)
# Output: optimal gamma, or full optimization details
xv.bench <- function(gamma.0, Phi, thatb, bencher, gamma.only=TRUE, ...) {
  # Use log(gamma) as the internal optimization variable to avoid needing
  # any constraints
  xv.objective <- function(log.gamma) {
    cv.score(gamma=exp(log.gamma), Phi=Phi, thatb=thatb, bencher=bencher, ...)
  }
  fit <- optim(par=log(gamma.0), fn=xv.objective, method="BFGS")
  # fit <- optim(par=log(gamma.0), fn=xv.objective, method="Brent", lower=log(1e-6), upper=log(1e3))
  if (gamma.only) {
    return(exp(fit$par))
  } else {
    return(fit)
  }
}

# Compute approximate cross-validation scores using the matrix of
# partial derivatives (a.k.a. hat matrix), and the approximations
# from Wahba (1990)
cv.score <- function(gamma, Phi, thatb, bencher, gcv=FALSE, ... ) {
  require(numDeriv)
  m <- nrow(Phi)
  stopifnot(ncol(Phi)==m)
  delta.hat <- function(y) {
    extract_estimate(bencher(Phi=Phi, gamma=gamma, thatb=y, ...))
  }
  hat.matrix <- jacobian(delta.hat, thatb)
  if (gcv) {
    residual.projection <- diag(m) - hat.matrix
    residuals.of.data <- residual.projection %*% thatb
    cv <- (t(residuals.of.data) %*% Phi %*% residuals.of.data)/(tr(residual.projection)^2)
    cv <- as.numeric(cv)
  } else {
    residuals <- (thatb - delta.hat(thatb))
    self.hat <- diag(hat.matrix)
    cv <- mean(diag(Phi)*residuals^2/(1-self.hat)^2)
  }
  return(cv)
}
######## Cross-validation ############
# Leave-one-out CV for picking the level of smoothing

# Take a matrix of weights and return the matrix with a selected row and
# column set to zero, for leaving out one data point
# Inputs: matrix (Phi), data-point (k)
# Output: Phi with row and column k zeroed out
loo.weights <- function(Phi,k) {
  stopifnot(nrow(Phi) >= k, ncol(Phi) >= k)
  Phi.less.k <- Phi
  Phi.less.k[k,] <- 0
  Phi.less.k[,k] <- 0
  return(Phi.less.k)
}

# Extract a particular fitted or estimated value from a complex
# benchmarking object
# Inputs: object containing estimates (estimate),
# index of the prediction/estimate to extract
# Output: the corresponding value
# Presumes: either estimate is a list with a component named delta, containing
# a vector of estimates, OR estimate is just a vector
extract_estimate <- function(estimate,k) {
  if ("delta" %in% names(estimate)) {
    return(estimate$delta[k])
  } else {
    return(estimate[k])
  }
}

# Calculate the cross-validation score for bench-marking functions
# Not intended for end users, but as a component to other functions
# Inputs:
# smoothing factor (gamma)
# matrix of loss weights (Phi),
# vector of Bayes estimates (thatb),
# the function which does benchmarking (bencher)
# other arguments to that function (...)
# Output: Phi-weighted out-of-sample squared error
xv.score <- function(gamma, Phi, thatb, bencher, ... ) {
  m <- nrow(Phi)
  stopifnot(ncol(Phi)==m)
  loo.Phis <- lapply(1:m, loo.weights, Phi=Phi)
  # TODO: Replace this for() loop with a proper vectorized expression
  loo.loses <- vector(length=m)
  for (area in 1:m) {
    loo.estimate <- bencher(Phi=loo.Phis[[area]], gamma=gamma, thatb=thatb, ...)
    loo.value <- extract_estimate(loo.estimate, area)
    loo.loses[area] <- Phi[area,area]*(thatb[area]-loo.value)^2
  }
  return(mean(loo.loses))
}




ResultsAnaModelBased_point<-function (True.mean, Est.mean){
  
  m<-dim(True.mean)[1]
  NoSim<-dim(True.mean)[2]
  True.mean<-t(True.mean)
  Est.mean<-t(Est.mean)
  
  
  assign("m",m,pos=1)
  RB <-rep(0,m)
  RRMSE <-rep(0,m)
  Bias_point<-rep(0,m)
  
  True.MSE <-rep(0,m)
  Est.MSE<-rep(0,m)
  CV.MSE<-rep(0,m)
  BIAS.MSE<-rep(0,m)
  
  for (i in 1:m){
    RB[i]<-mean(((Est.mean[,i])-(True.mean[,i]))/(True.mean[,i]))
    Bias_point[i]<-((mean(Est.mean[,i])-mean(True.mean[,i])))
    RRMSE[i]<-sqrt(mean((((Est.mean[,i])-(True.mean[,i]))/(True.mean[,i]))^2))
    True.MSE[i]<-mean((Est.mean[,i]-True.mean[,i])^2)
  }
  
  True.RootMSE=sqrt(True.MSE)
  
  
  
  list(RB=RB,RRMSE=RRMSE,Bias_point=Bias_point,True.RMSE=True.RootMSE)
}


build_laplacian <- function(q) {
  q_r <- diag(rowSums(q))
  q_c <- diag(colSums(q))
  return(q_r + q_c - 2*q)
}