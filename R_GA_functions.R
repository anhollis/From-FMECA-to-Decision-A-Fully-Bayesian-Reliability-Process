# functions used in the GA function

# function to perform "crossover" aka "recombination"
crossover <- function(current_pop, crit)
{
	# takes as arguments the current population and the criteria values
   # current_pop (and crit) are sorted from best to worst
   child = 0
   while (sum(child)==0)
   {
	M = nrow(current_pop)		  # identify size of current population
	ncomps = ncol(current_pop)     # identify number of comps in system
	parIDs = sample(1:M,2,prob=(M-(1:M)+1)/(M*(M+1)/2)) 
		# select 2 parents at random (with prob inverse of rank, Bayes Reliability p. 322)
	pars = current_pop[parIDs,]
	chromo = sample(1:2,ncomps,replace=T)      # randomly select which parent contributes to each component
	child = pars[cbind(chromo,1:ncomps)]   # create child
	}
   return(child)
}

# function to "repair" candidates that exceed the budget
repairDown<-function(cand, cost, bud)
{
  # takes a candidate allocation, the cost of all comps, and the budget
  # if the total for this candidate exceeds the budget, tests for components
  # are decreased at random until the cost satisfies the budget
  # can yield a candidate with a cost slightly below the budget

  total = sum(cand*cost)
  nonzero=which(cand!=0)  # identify which components are being tested
  n=length(nonzero)       # identify how many components are being tested
  while (total > bud)     # loop while the total cost of candidate exceeds budget
  {
	i = sample(1:n,1,prob=cand[nonzero]/sum(cand))     # randomly select a component that is being tested
	cand[nonzero[i]] = cand[nonzero[i]]-1    # decrease the number of tests on comp by 1
	total = sum(cand*cost)                   # calculate total of modified candidate
 	nonzero=which(cand!=0)  # identify which components are being tested
  	n=length(nonzero)       # identify how many components are being tested
  }
  return(cand)
}


# function to add tests to allocation if the cost is well below the budget
repairUP<-function(cand,cost,bud)
{
  total = sum(cand*cost)
  nonzero=which(cand!=0)  # identify which components are being tested
  n=length(nonzero)       # identify how many components are being tested
  while (total < bud)     # loop while the total cost of candidate exceeds budget
  {
	i = sample(1:n,1)     # randomly select a component that is being tested
	cand[nonzero[i]] = cand[nonzero[i]]+1    # increase the number of tests on comp by 1
	total = sum(cand*cost)                   # calculate total of modified candidate
  }
  return(cand)
}

# function to generate mutation solutions
mutate<-function(cand, cost, bud, g, constraints, mu=0.01, sig=1,L=rep(0,length(cand)))
{
	# pass a single candidate (cand)
	cand = as.numeric(cand)
	nonzero=which(cand!=0)  # identify which components are being tested
	n=length(nonzero)       # identify how many components are being tested

	#U=floor(bud/cost) + 0.001	# calculate the max number of tests possible on each component
								# problem occurs when max number of tests is achieved; add a small amount so logit not affected
							# note that default value of min number of tests is 0 (user can modify)
	U=constraints+0.001
	z=a=u=rep(0,length(cand))
	z[nonzero]=(cand[nonzero]-L[nonzero])/(U[nonzero]-L[nonzero])
	a[nonzero] = log(z[nonzero]/(1-z[nonzero]))+(runif(n)-0.5)*sig*exp(-mu*g)
	u[nonzero] = floor(L[nonzero] + (U[nonzero] - L[nonzero])*exp(a[nonzero])/(1+exp(a[nonzero])))
	return(u)
}

mult<-function(a,b){return(a*b)}

# function to check if allocations are on the three
# criteria Pareto front
checkon=function(newpt,newdes,curpf,curpfdes)
{
  newpt_comp<-as.numeric(newpt)
  crit_names<-colnames(curpf)
  ncrit<-length(newpt)
  ncomps=length(newdes)
  if(is.null(dim(curpf)) | is.null(dim(curpfdes))){
    curpf=matrix(curpf,ncol=ncrit,byrow = T)
    curpfdes=matrix(curpfdes,ncol=ncomps,byrow=T)
  }
	## this needs to be changed based on the interest 
	## of maximizing or minimizing the criteria
  better<-newpt_comp[1]<curpf[,1]
  notWorse=newpt_comp[1]<=curpf[,1]
  worse<-newpt_comp[1]>curpf[,1]
  notBetter<-newpt_comp[1]>=curpf[,1]
  equal<-newpt_comp[1]==curpf[,1]
  for(i in 2:ncrit){
    better=cbind(better,newpt_comp[i]<curpf[,i])
    notWorse=cbind(notWorse,newpt_comp[i]<=curpf[,i])
    worse<-cbind(worse,newpt_comp[i]>curpf[,i])
    notBetter<-cbind(notBetter,newpt_comp[i]>=curpf[,i])
    equal<-cbind(equal,newpt_comp[i]==curpf[,i])
  }
  
  cond1<-rep(0,nrow(curpf))
  for(i in 1:ncrit){
    temp<-notWorse
    temp[,i]<-better[,i]
    cond1=cond1+apply(temp,1,prod)
  }
  cond1=cond1==0
  
  cond2<-rep(0,nrow(curpf))
  for(i in 1:ncrit){
    temp<-notBetter
    temp[,i]<-worse[,i]
    cond2=cond2+apply(temp,1,prod)
  }
  cond2=cond2+apply(equal,1,prod)
  cond2=sum(cond2)

	newpf=curpf[cond1,]
	newpfdes=curpfdes[cond1,]
	if(cond2==0)
	{
		newpf=rbind(newpf,newpt)
		newpfdes=rbind(newpfdes,newdes)
	}
	
	if(is.null(dim(newpf)) & is.null(dim(newpfdes))){
	  newpf=matrix(newpf,ncol=ncrit,byrow = T)
	  newpfdes=matrix(newpfdes,ncol=ncomps,byrow=T)
	}
	
	return(list(newpf,newpfdes))
}


