#==============================================================================
# SUBJECT:  Fay-Herriot Function
# AUTHOR:   Timo Schmid  December 18, 2015
# based on Pratesi and Salvati (2009)
#------------------------------------------------------------------------------
#==============================================================================

# Description:
# The function provides estimates of a Fay-Herriot model
# Point estimates and analytic MSE
# Compare Pratesi and Salvati (2009)
# The function uses the transformation of Carter and Rolph (1974)
# MSE estimation not valid

# Open points 
# Implement out-of sample prediction
# Implement directly the benchmarking
# Implement directly the MSE estimation for the benchmarked FH


# # Input for function
# # formula = formula object to specify the relationship between y and x
# # vardir = Approx. the variance of the direct estimator vardir_i = 1/(4*n_i)
# # dataframe_sample = dataframe with the covariates x and y
# # dataframe_pop_aux = dataframe with the covariates x from the population
# # saind = areaid for the sampled units
# # x.total_saind = areaid for the population
# # long = longitude coordinate
# # lat = latitude coordinate


# formula=y~x
# vardir=vardir
# dataframe_sample<-data.frame(y=direct,x=x.n)
# saind=1:m
# dataframe_pop_aux<-dataframe_sample
# x.total_saind<-1:m
# W_weight=W
# # 
#timo<-FH_freq(y~x,vardir,dataframe_sample=data.frame(y=direct,x=x.n),saind=1:m,dataframe_pop_aux=data.frame(y=direct,x=x.n),
#                     x.total_saind=1:m,W_weight=W)

FH_freq_transnew<-function(formula, vardir, dataframe_sample, saind, 
  dataframe_pop_aux, x.total_saind, area_count, trans=FALSE, Boot=FALSE, B=100)
{
  
  library(MASS)
  library(nlme)
  library(spgwr)
  library(formula.tools)
  
  # log-likelihood function 
  logl=function(delta,vardir_logl,areanumber_logl,direct_logl,x_logl){ 
    psi=matrix(c(vardir_logl),areanumber_logl,1)
    Y=matrix(c(direct_logl),areanumber_logl,1)
    X=x_logl
    Z.area=diag(1,areanumber_logl)
    sigma.u_log<-delta[1]
    I<-diag(1,areanumber_logl)
    #V is the variance covariance matrix
    V<-sigma.u_log*Z.area%*%t(Z.area)+I*psi[,1]
    Vi<-solve(V)
    Xt=t(X)
    XVi<-Xt%*%Vi
    Q<-solve(XVi%*%X)
    P<-Vi-(Vi%*%X%*%Q%*%XVi)
    b.s<-Q%*%XVi%*%Y
    
    
    ee=eigen(V)
    -(areanumber_logl/2)*log(2*pi)-0.5*sum(log(ee$value))-(0.5)*log(det(t(X)%*%Vi%*%X))-(0.5)*t(Y)%*%P%*%Y
  }
  
  # Definition of vectors
  ni_sampled<-(table(saind))
  n<-length(saind)
  areanumber_sample<-length(ni_sampled)
  
  
  areanumber<-length(unique(x.total_saind))
  
  ni_ind<-rep(0,areanumber)
  
  for(i in 1:areanumber){
    ni_ind[i]<- max((as.numeric((attr(table(x.total_saind),"dimnames"))$x.total_saind)[i]==
                       as.numeric(attr(ni_sampled,"dimnames")$saind)))
  }
  
  ni<-rep(0,areanumber)
  ni[as.logical(ni_ind)]<-ni_sampled
  
  makeXY <- function(formula, data){
    mf <- model.frame(formula=formula, data=data)
    x <- model.matrix(attr(mf, "terms"), data=mf)
    y <- model.response(mf)
    
    list(y = y,
         x = x)
  }
  daten<-makeXY(formula=formula,data=dataframe_sample)
  y<-daten$y
  x<-daten$x
  p<-ncol(x)
  

  
  # Transformation
  if(trans==TRUE){
    y<-asin(sqrt(y))
    vardir <-  1/(4*area_count) ## changed!!
    }
  
  # Fit of the model to the data
  ottimo=optimize(logl,c(0.001,100),maximum = TRUE,
                vardir_logl=vardir,areanumber_logl=areanumber_sample,direct_logl=y,x_logl=x)
  
  estsigma2u<-ottimo$maximum
  
  
  # Computation of the coefficients'estimator (Bstim)
  D=diag(1,areanumber_sample)
  V<-estsigma2u*D%*%t(D)+diag(as.numeric(vardir))
  Vi<-solve(V)
  Q<-solve(t(x)%*%Vi%*%x)
  Beta.hat<-Q%*%t(x)%*%Vi%*%y
  
  # Computation of the EBLUP
  res<-y-c(x%*%Beta.hat)
  I<-diag(1,areanumber_sample)
  Sigma.u=estsigma2u*I
  u.hat=Sigma.u%*%t(D)%*%Vi%*%res
  # Small area mean 
  est_mean<-x%*%Beta.hat+D%*%u.hat
  

  
  # Out-of-sample-prediction
  dataframe_pop_aux<-data.frame(dataframe_pop_aux,helper=rnorm(1,0,1))
  lhs(formula)<-quote(helper)
  daten.pop<-makeXY(formula=formula,data=dataframe_pop_aux)
  x.total<-daten.pop$x
  
  # Synthetic prediction for out-of-sample
  pred_out<-as.numeric(as.vector(Beta.hat)%*%t(x.total))
  
  est_mean_final<-rep(NA,areanumber)
  
  # Merge in-sample and out-of-sample (out and in sample)
  est_mean_final[as.logical(ni_ind)]<-est_mean[1:areanumber_sample]
  est_mean_final[as.logical(abs(ni_ind-1))]<-pred_out[as.logical(abs(ni_ind-1))]
  
  # Computation of the shrinkage factor
  gamma <- estsigma2u/(estsigma2u+vardir)
  
  # Truncation
  if(trans==TRUE){
  est_mean_final[est_mean_final<0]<-0
  est_mean_final[est_mean_final>(pi/2)]<-(pi/2)
  
  # Back-transformation
  est_mean_final<-(sin(est_mean_final))^2
  }
  
  # Extend the random effects
  u.hat_new<-rep(0,areanumber)
  u.hat_new[as.logical(ni_ind)]<-u.hat
  
  #--------------------------
  # MSE estimation
  est_mse<-rep(NA,areanumber)
  
  if(Boot==TRUE){
  # Bootstrap
  boots_est<-matrix(NA,areanumber,B)
  boots_par<-matrix(NA,areanumber,B)
  t_boot<-matrix(NA,areanumber_sample,B)
  
  for (b in 1:B){
    
    v_boot<-rnorm(areanumber,0,sqrt(estsigma2u)) # why square-root, compare to paper!
    e_boot<-rnorm(areanumber_sample,0,sqrt(vardir)) # same comment
    Xbeta_boot<-pred_out
    
    # Truncation
    if(trans==TRUE){
      true_value_boot<-Xbeta_boot+v_boot
      true_value_boot[true_value_boot<0]<-0
      true_value_boot[true_value_boot>(pi/2)]<-(pi/2)
      
      # Back-transformation
      true_value_boot<-(sin(true_value_boot))^2
      boots_par[,b]<-true_value_boot
    }else{
      boots_par[,b]<-Xbeta_boot+v_boot
    }
    
      
    ystar<-Xbeta_boot[as.logical(ni_ind)]+v_boot[as.logical(ni_ind)]+e_boot
    theta_star<-Xbeta_boot[as.logical(ni_ind)]+v_boot[as.logical(ni_ind)] # according to paper

    # Estimation of beta_boot
    ottimo_boot=optimize(logl,c(0.001,100),maximum = TRUE,
                    vardir_logl=vardir,areanumber_logl=areanumber_sample,direct_logl=ystar,x_logl=x)
    
    estsigma2u_boot<-ottimo_boot$maximum
    
    # Computation of the coefficients'estimator (Bstim)
    D=diag(1,areanumber_sample)
    V<-estsigma2u_boot*D%*%t(D)+diag(as.numeric(vardir))
    Vi<-solve(V)
    Q<-solve(t(x)%*%Vi%*%x)
    Beta.hat_boot<-Q%*%t(x)%*%Vi%*%ystar
    
    # Computation of the EBLUP
    res_boot<-ystar-c(x%*%Beta.hat_boot)
    Sigma.u=estsigma2u_boot*I
    u.hat=Sigma.u%*%t(D)%*%Vi%*%res_boot
    # Small area mean 
    est_mean_boot<-x%*%Beta.hat_boot+D%*%u.hat
    
    # Synthetic prediction for out-of-sample
    pred_out_boot<-as.numeric(as.vector(Beta.hat_boot)%*%t(x.total))
    
    est_mean_final_boot<-rep(NA,areanumber)
    
    # Merge in-sample and out-of-sample (out and in sample)
    est_mean_final_boot[as.logical(ni_ind)]<-est_mean_boot[1:areanumber_sample]
    est_mean_final_boot[as.logical(abs(ni_ind-1))]<-pred_out_boot[as.logical(abs(ni_ind-1))]
    
    # Computation of the shrinkage factor
    gamma_boot <- estsigma2u_boot/(estsigma2u_boot+vardir)

    # Calculation of pivotal quantity
    t_boot[,b] <- (theta_star-est_mean_boot)/sqrt(vardir*(1-gamma_boot))
    
    # Truncation
    if(trans==TRUE){
      est_mean_final_boot[est_mean_final_boot<0]<-0
      est_mean_final_boot[est_mean_final_boot>(pi/2)]<-(pi/2)
      
      # Back-transformation
      est_mean_final_boot<-(sin(est_mean_final_boot))^2
      boots_est[,b]<-est_mean_final_boot ## changed!!
    }else{
      boots_est[,b]<-est_mean_final_boot
    }

    cat("\r Bootstrap ",b, "of", B, "Completed") ## changed!!
    flush.console() ## changed!!
  }
  
                # Parametric confidence intervals boundaries l and u
                ci_quant <- t(apply(t_boot,1,quantile,probs=c(0.025, 0.975)))
                ci_l <- est_mean+ci_quant[,1]*sqrt(vardir*(1-gamma)) # vardir right here?
                ci_u <- est_mean+ci_quant[,2]*sqrt(vardir*(1-gamma))
                  
                # Truncation of confidence intervals
                ci_l[ci_l<0]<-0
                ci_u[ci_u>(pi/2)]<-(pi/2)
                  
                # Back-transformation of confidence intervals
                ci_l<-(sin(ci_l))^2  
                ci_u<-(sin(ci_u))^2  
  
  # Calculation of the MSE of the estimates
  Quality_MSE<-function(estimator, TrueVal){
    RMSE<-rep(NA,dim(estimator)[1])
    for(ii in 1:dim(estimator)[1]){
      RMSE[ii]<-(1/B*sum(((estimator[ii,]-TrueVal[ii,]))^2))
    }
    return(RMSE)
  }
  
  est_mse<-Quality_MSE(estimator=boots_est,TrueVal=boots_par)
  
  }
  
  # Truncation and Back-transformation of in-sample predictions

  if(trans==TRUE){
    est_mean[est_mean<0]<-0
    est_mean[est_mean>(pi/2)]<-(pi/2)
    est_mean<-(sin(est_mean))^2
  }

  
  
  # List of Results
#   list(coefficients = Beta.hat,sigma2v=estsigma2u,samplesize = ni,rand.eff=u.hat_new,
#        est_mean=as.numeric(est_mean_final),MSE_mean=as.numeric(est_mse),Transformation=trans )

  list(coefficients = Beta.hat,sigma2v=estsigma2u,samplesize = ni,rand.eff=u.hat_new,
       fitted = data.frame(level = saind,rest=D%*%u.hat), X = x, ## changed!!
       est_mean_final=data.frame(level = x.total_saind,pred = as.numeric(est_mean_final)), ## changed!!
       mse = data.frame(level = x.total_saind, mse=as.numeric(est_mse)), ## changed!
       # ci = data.frame(level = saind,pred_in = as.numeric(est_mean), ci_l = ci_l, ci_u = ci_u),
       Transformation=trans
  )
  
  } 
