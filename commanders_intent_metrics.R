pareto <- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/prior2_phase2/GA_res/pareto.csv")
#load("C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\artillery_sim\\prior1_samples")
load("C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\artillery_sim\\prior2_samples")
#cost=c(rep(5,7),81,81,81,81)
cost<-c(rep(5,7),10,10,10,10)
designs<-pareto[,1:11]

prob_weights<-as.numeric(apply(prior2_samples[,15:21],2,mean))
red_weights<-as.numeric(apply(prior2_samples[,26:32],2,mean))

entropy<-function(design,weights=rep(1,7)){
  function_design<-get_function_counts(design)+0.0000001
  props<-function_design/sum(function_design)
  return(-sum(weights*props*log(props)))
}

get_function_counts<-function(design){
  design=as.numeric(design)
  function_counts<-design[1:7]
  if(design[8]>0){
    design[1:3]=design[1:3]+design[8]
  }
  if(design[9]>0){
    design[4:5]=design[4:5]+design[9]
  }
  if(design[10]>0){
    design[6:7]=design[6:7]+design[10]
  }
  if(design[11]>0){
    design[1:7]=design[1:7]+design[9]
  }
  return(design[1:7])
}

pareto$cost<-rep(0,nrow(pareto))
pareto$diversity<-rep(0,nrow(pareto))
pareto$likely_fail<-rep(0,nrow(pareto))
pareto$likely_severe_fail<-rep(0,nrow(pareto))

for(i in 1:nrow(pareto)){
  pareto$cost[i]=sum(cost*designs[i,])
  pareto$diversity[i]<-entropy(designs[i,])
  pareto$likely_fail[i]<-entropy(designs[i,],prob_weights)
  pareto$likely_severe_fail[i]<-entropy(designs[i,],red_weights)
}
write.csv(pareto,"~/USMA Project/MORS_paper/code/artillery_sim/prior2_phase2/GA_res/commander_metrics.csv")
