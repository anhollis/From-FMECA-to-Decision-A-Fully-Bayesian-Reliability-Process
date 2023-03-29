#Example Script of whole process

#Get Design candidates using GA
source('~/USMA Project/MORS_paper/code/artillery_sim/GA_R.R')
source('~/USMA Project/MORS_paper/code/artillery_sim/generate_data.R')
load("C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\artillery_sim\\prior1_samples")
load("C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\artillery_sim\\step1\\prior1_phase1\\GA_res\\start")

mcmcoutput=prior1_samples[,c(1:25,36)]
node_names=colnames(mcmcoutput)
startgen=start[,1:26]
start_crit=start[,27:28]
crit_names=c("System","red_System")
weights=c(0.5,0.5)
cost=c(rep(0,14),rep(5,7),rep(10,5))
constraints=c(rep(0,14),rep(10,7),rep(0,5))
bud=80
nOut = 200
nDen=512
directory="C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\artillery_sim\\mein_herz\\"


ga_allR(mcmcoutput, node_names, crit_names, startgen, start_crit, weights, 
        cost, bud, constraints, nOut = 100, nDen=400,directory)


#Get Pareto Designs

#run process_pareto.R
pareto <- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/mein_herz/pareto.csv")
#run commanders_intent_metrics.R to get set of additional metrics to use in deciding between pareto candidate designs.

final_design=pareto[7,2:12]


#Generate Data from Design
collected_data=generate_data(final_design)
write.csv(collected_data,"C:\\Users\\anhol\\Documents\\USMA Project\\MORS_paper\\code\\artillery_sim\\mein_herz\\collected_data.csv")

#Generate from posterior
dat<- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/mein_herz/collected_data.csv")
sample_res<-mh_sampler(data=dat,log_prior=log_prior,log_lik=log_lik,N=1500000,burn=500000)
