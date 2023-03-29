library(coda)
dat11<- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/step1/prior1_phase1/collected_data.csv")
dat12<- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/step1/prior1_phase2/collected_data.csv")
dat21<- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/step1/prior2_phase1/collected_data.csv")
dat22<- read.csv("~/USMA Project/MORS_paper/code/artillery_sim/step1/prior2_phase2/collected_data.csv")

sample_res<-mh_sampler(data=dat11,log_prior=log_prior,log_lik=log_lik,N=1500000,burn=500000)


thin_idx<-((1:10000)-1)*100+1
thinned_sample<-sample_res$samples[thin_idx,]
geweke.diag(thinned_sample)
effectiveSize(thinned_sample)
par(mfrow=c(2,3))
for(i in 1:14){
  plot(1:nrow(thinned_sample),thinned_sample[,i],type="l")
}

for(i in 1:14){
  acf(thinned_sample[,i])
}
