library(truncnorm)
source('~/USMA Project/MORS_paper/code/system_fails_from_fail_modes.R')
source('~/USMA Project/MORS_paper/code/true_sim.R')

create_failure_table<-function(failure_mode_probs){
  failure_mode_IDs<-c("PropellantDoesNotIgnite","PropellantSnuffsOut",
                      "Blowby_projectile","Blowby_breech",
                      "NotEnoughPressure","TooMuchPressure",
                      "FailToInitiatePower","InsufficientBatterySupply",
                      "MotorDamage","InadequateTorque",
                      "GEUCommandNotRecieved","CapacitorNotCharged",
                      "CommandSentEarly","CommandSentLate")
  function_IDs<-c("IgnitePropellant","SealGas","GeneratePressure",
                  "InitiateInflightPower","ModifyAttitude",
                  "ArmWarheadFuze","SendFireCommand")
  subsystem_IDs<-c("Launch","BallisticFlight","TerminalEffects")
  
  system_fails<-system_fails_from_failure_modes(failure_mode_probs)
  
  failure_table<-data.frame(comp_name=c(failure_mode_IDs,function_IDs,subsystem_IDs,"System"),
                            probs=c(failure_mode_probs,system_fails))
  return(failure_table)
}

log_lik<-function(failure_mode_probs,data){
  n<-nrow(data)
  failure_prob_table<-create_failure_table(failure_mode_probs)
  log_lik=0
  for(i in 1:n){
    prob_idx<-which(as.character(failure_prob_table$comp_name)==as.character(data$name[i]))
    p=failure_prob_table$probs[prob_idx]
    fails<-data$n_fails[i]
    total=data$n_total[i]
    log_lik=log_lik+fails*log(p)+(total-fails)*log(1-p)
  }
  return(log_lik)
}

log_prior<-function(failure_mode_probs){
  r1_alpha<-0.2953941
  r1_beta<-25.2289711
  r3_alpha<-1.521054
  r3_beta<-3.648332 
  alphas<-c(rep(r1_alpha,2),r3_alpha,
            rep(r1_alpha,5),rep(r3_alpha,4),
            rep(r1_alpha,2))
  betas<-c(rep(r1_beta,2),r3_beta,
           rep(r1_beta,5),rep(r3_beta,4),
           rep(r1_beta,2))
  logprior<-(alphas-1)*log(failure_mode_probs)+(betas-1)*log(1-failure_mode_probs)
  return(sum(logprior))
}



mh_sampler<-function(data,init=NULL,log_prior,jump=NULL,log_lik,N=110000,burn=10000){
  init<-c(rep(0.01157302,2),0.2942427,
          rep(0.01157302,5),rep(0.2942427,4),
          rep(0.01157302,2))
  log_post<-function(failure_modes,data){
    return(-log_prior(failure_modes)-log_lik(failure_modes,data))
  }
  #start<-optim(init,log_post,gr=NULL,data=data,method="L-BFGS-B",lower=rep(0.00001,14),upper=rep(0.99999,14))
  #init=start$par
  jump<-c(rep(0.05,2),100,rep(0.05,5),
          rep(100,4),rep(0.08,2))
  p<-length(init)
  acceptance_rate<-rep(0,p)
  theta_samples<-matrix(0,ncol=p,nrow=N)
  theta_samples[1,]=init
  for(i in 2:N){
    if(i%%100==0){print(i)}
    #print(i)
    old_theta<-theta_samples[i-1,]
    new_theta<-rep(0,p)
    for(j in 1:p){
      prop_j<-rtruncnorm(1,a=0,b=1,mean=old_theta[j],sd=jump[j])
      if(j==1){
        prop_all<-c(prop_j,old_theta[(j+1):p])
      }else if(j==p){
        prop_all<-c(new_theta[1:(j-1)],prop_j)
      }else{
        prop_all<-c(new_theta[1:(j-1)],prop_j,old_theta[(j+1):p])
      }
      hastings_term=log(dtruncnorm(old_theta[j],a=0,b=1,mean=prop_j,sd=jump[j]))-log(dtruncnorm(prop_j,a=0,b=1,mean=old_theta[j],sd=jump[j]))
      R=(log_prior(prop_all)+log_lik(prop_all,data))-(log_prior(old_theta)+log_lik(old_theta,data))+hastings_term
      U<-log(runif(1,0,1))
      if(U<R){
        new_theta[j]=prop_j
        acceptance_rate[j]=acceptance_rate[j]+1
      }else{
        new_theta[j]=old_theta[j]
      }
    }
    theta_samples[i,]=new_theta
  }
  return(list(samples=theta_samples[(burn+1):N,],
              acceptance=acceptance_rate/N))
}