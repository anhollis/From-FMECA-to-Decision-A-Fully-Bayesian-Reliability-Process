source('~/USMA Project/MORS_paper/code/system_fails_from_fail_modes.R')
prior_simulation<-function(N=1000){
  r1_alpha<-0.2953941
  r1_beta<-25.2289711 
  r3_alpha<-1.521054
  r3_beta<-3.648332
  n_failure_modes=14
  col_names<-c("PropellantDoesNotIgnite","PropellantSnuffsOut",
               "Blowby_projectile","Blowby_breech",
               "NotEnoughPressure","TooMuchPressure",
               "FailToInitiatePower","InsufficientBatterySupply",
               "MotorDamage","InadequateTorque",
               "GEUCommandNotRecieved","CapacitorNotCharged",
               "CommandSentEarly","CommandSentLate",
               "IgnitePropellant","SealGas","GeneratePressure",
               "InitiateInflightPower","ModifyAttitude",
               "ArmWarheadFuze","SendFireCommand",
               "Launch","BallisticFlight","TerminalEffects", "System")
  n_components<-length(col_names)
  
  prior_samples<-matrix(rep(0,n_failure_modes*N),nrow=N)
  failure_mode_r1<-rbeta(N*9,r1_alpha,r1_beta)
  failure_mode_r3<-rbeta(N*5,r3_alpha,r3_beta)
  prior_samples[,c(1:2,4:8,13:14)]=matrix(failure_mode_r1[1:(N*9)],nrow=N,ncol=9)
  prior_samples[,c(3,9:12)]=matrix(failure_mode_r3[1:(N*5)],nrow=N,ncol=5)
  
  other_failure_probs<-t(apply(prior_samples[,1:14],1,system_fails_from_failure_modes))
  
  prior_samples=cbind(prior_samples,other_failure_probs)
  colnames(prior_samples)=col_names
  return(prior_samples)
}

