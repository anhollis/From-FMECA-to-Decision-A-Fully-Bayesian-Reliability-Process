#Compute Probability of Red, Yellow, and Green Failure

#This function takes in a vector of failure mode failure probabilities and produces
#the probabilitiy of green, yellow, and red failure at the functional, subsystem, and system levels.

compute_p_ryg_system_comp<-function(failure_mode_probs){
  failure_mode_sev=c("green","red","yellow","red","red","red",
                     "yellow","yellow","red","red","green",
                     "green","green","green")
  failure_mode_func_map<-c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
  failure_mode_sub_map<-c(1,1,1,1,1,1,2,2,2,2,3,3,3,3)
  function_ryg<-matrix(rep(0,7*3),ncol=3,byrow=T)
  sub_ryg<-matrix(rep(0,3*3),ncol=3,byrow=T)
  for(i in 1:7){
    idx<-which(failure_mode_func_map==i)
    function_ryg[i,]=compute_p_ryg(failure_mode_probs[idx],
                                  failure_mode_sev[idx])
  }
  for(i in 1:3){
    idx<-which(failure_mode_sub_map==i)
    sub_ryg[i,]=compute_p_ryg(failure_mode_probs[idx],
                                  failure_mode_sev[idx])
  }
  system_ryg=compute_p_ryg(failure_mode_probs,failure_mode_sev)
  names(system_ryg)=c("green","yellow","red")
  colnames(sub_ryg)=c("green","yellow","red")
  colnames(function_ryg)=c("green","yellow","red")
  rownames(function_ryg)=c("IgnitePropellant","SealGas","GeneratePressure",
                           "InitiateInflightPower","ModifyAttitude","ArmWarheadFuze","SendFireCommand")
  rownames(sub_ryg)=c("Launch","BallisticFlight","TerminalEffects")
  return(list(function_ryg=function_ryg,sub_ryg=sub_ryg,system_ryg=system_ryg))
}

compute_p_ryg<-function(failure_mode_probs,sev){
  sev_dat<-data.frame(failure_mode_probs,sev)
  red_probs=sev_dat[which(sev_dat[,2]=="red"),1]
  yellow_probs=sev_dat[which(sev_dat[,2]=="yellow"),1]
  green_probs=sev_dat[which(sev_dat[,2]=="green"),1]
  prob_red=1-prod(1-red_probs)
  prob_yellow=(1-prob_red)*(1-prod(1-yellow_probs))
  prob_green=(1-prob_red)*(prod(1-yellow_probs))*(1-prod(1-green_probs))
  return(c(prob_green,prob_yellow,prob_red))
}
