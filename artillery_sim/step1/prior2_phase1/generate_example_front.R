source('~/my_programs/artillery_sim/eval.R')
source('~/my_programs/artillery_sim/GA_R.R')
source('~/my_programs/artillery_sim/R_GA_functions.R')
load('~/my_programs/artillery_sim/prior2_samples')

library(MCMCpack)
library(lhs)
prior_samples=prior2_samples[,c(1:25,36)]
#prior_samples<-system_probs(prior_samples[,1:14])
budget=80
M=100
cost=c(rep(81,14),rep(5,7),81,81,81,81,81)
constraints<-c(rep(0,14),rep(10,7),rep(0,3),0,0)
das<-randomLHS(M,21)
random_start=matrix(rep(0,M*26),ncol=26)
for(i in 15:21){
  random_start[,i]=round(qunif(das[,i],0,10))
}
#generator<-c(rep(5,14),rep(5,7),rep(0,3),0,0)
#random_start=round(rdirichlet(30,generator)*budget)
#for(i in 1:30){
#  random_start[i,]=round(random_start[i,]/cost)
#}
#random_start<-t(apply(random_start,1,repairUP,cost=cost,bud=budget))
random_start<-t(apply(random_start,1,repairDown,cost=cost,bud=budget))
#random_start<-t(apply(random_start,1,fix_constraints,constraints=constraints))
sys_names<-c("PropellantDoesNotIgnite","PropellantSnuffsOut",
             "Blowby_projectile","Blowby_breech",
             "NotEnoughPressure","TooMuchPressure",
             "FailToInitiatePower","InsufficientBatterySupply",
             "MotorDamage","InadequateTorque",
             "GEUCommandNotRecieved","CapacitorNotCharged",
             "CommandSentEarly","CommandSentLate",
             "IgnitePropellant","SealGas","GeneratePressure",
             "InitiateInflightPower","ModifyAttitude",
             "ArmWarheadFuze","SendFireCommand",
             "Launch","BallisticFlight","TerminalEffects", "System","red_system")
colnames(random_start)=sys_names
start_cw<-eval_CW(prior_samples,sys_names,random_start,sys_names[25:26],nOut=100)
start=cbind(random_start,start_cw)
colnames(start)[27:28]=c('system_cw',"red_system_cw")
save(start,file='~/my_programs/artillery_sim/prior2_phase1/start')
weight_levels<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
for(i in 1:length(weight_levels)){
  ga_allR(prior_samples,sys_names,sys_names[25:26],random_start,start_cw, 
          c(weight_levels[i],1-weight_levels[i]), cost, budget,constraints,
          nOut = 100, nDen=512,
          directory='~/my_programs/artillery_sim/prior2_phase1/GA_res/')
}