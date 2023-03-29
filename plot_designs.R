par(mfrow=c(2,2))
labels=c("F1","F2","F3","F4","F5","F6","F7",
         "Sub1","Sub2","Sub3","S")
#Prior 1, Phase1
n<-c(0,2,0,0,7,6,0,0,0,0,0)
barplot(n,
        names.arg=labels,col="blue",ylim=c(0,7),
        main="Prior 1, Phase 1")

#Prior 1, Phase2
n<-c(0,1,0,0,3,3,0,0,2,1,1)
barplot(n,
        names.arg=labels,col="blue",ylim=c(0,7),
        main="Prior 1, Phase 2")


#Prior 2, Phase1
n<-c(3,4,3,3,1,0,2,0,0,0,0)
barplot(n,
        names.arg=labels,col="blue",ylim=c(0,7),
        main="Prior 2, Phase 1")

#Prior 2, Phase2
n<-c(2,3,1,0,1,0,1,3,0,1,0)
barplot(n,
        names.arg=labels,col="blue",ylim=c(0,7),
        main="Prior 2, Phase 2")
