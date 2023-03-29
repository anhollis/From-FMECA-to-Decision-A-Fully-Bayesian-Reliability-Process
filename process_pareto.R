source('~/USMA Project/MORS_paper/code/R_GA_functions.R')
pareto <- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/prior2_phase1/GA_res/pareto.txt", sep="")
current_pop<-pareto[,1:27]
crit<-pareto[,28:29]

PFcandsCrit=crit[1,]
PFcands=current_pop[1,]

for (i in 2:nrow(crit))
{
  temp = checkon(crit[i,],current_pop[i,], PFcandsCrit, PFcands)
  PFcandsCrit = temp[[1]]
  PFcands = temp[[2]]
}
pareto=cbind(PFcands,PFcandsCrit)

pareto <- pareto[,c(16:26,28,29)]
write.csv(pareto,"~/USMA Project/MORS_paper/code/artillery_sim/prior2_phase1/GA_res/pareto.csv")
