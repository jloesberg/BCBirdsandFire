library(MCMCglmm)


x<-read.csv("Dataset-S2.csv")
x$PhyloName<-gsub(" ", "_",x$PhyloName)

#prepare the data
if(sum(is.na(x$PhyloName)>0)){x<-x[-which(is.na(x$PhyloName)),]}
if(sum(is.na(x$Loc.Artificial)>0)){x<-x[-which(is.na(x$Loc.Artificial)),]}
if(sum(x$Loc.Artificial=="u")>0){x<-x[-which(x$Loc.Artificial=="u"),]}
if(sum(is.na(x$Body.mass..log.)>0)){x<-x[-which(is.na(x$Body.mass..log.)),]}
if(sum(is.na(x$HWI)>0)){x<-x[-which(is.na(x$HWI)),]}
if(sum(is.na(x$Latitude)>0)){x<-x[-which(is.na(x$Latitude)),]}
if(sum(is.na(x$AnnualTemp)>0)){x<-x[-which(is.na(x$AnnualTemp)),]}
if(sum(is.na(x$TempRange)>0)){x<-x[-which(is.na(x$TempRange)),]}
if(sum(is.na(x$AnnualPrecip)>0)){x<-x[-which(is.na(x$AnnualPrecip)),]}
if(sum(is.na(x$PrecipRange)>0)){x<-x[-which(is.na(x$PrecipRange)),]}

dups<-names(which(table(x$PhyloName)>1))
todelete<-rep(0,dim(x)[1])

for(i in 1:length(dups)){
  if(sum(x$phylo.fidelity[which(x$PhyloName==dups[i])])>0){todelete[intersect(which(x$PhyloName==dups[i]), which(x$phylo.fidelity==0))]<-1}
  if(sum(x$phylo.fidelity[which(x$PhyloName==dups[i])])==0){todelete[which(x$PhyloName==dups[i])[-1]]<-1} #arbitrarily take the first one
}

x<-x[-which(todelete==1),]



trees<-read.tree("100birdtrees.tre") #there are 10,000 topologies in the posterior distribution. this is a random hundred of them.
t100<-trees[1:100] #we're renaming them in case something goes wrong; if your code is perfect, you can delete this line

tree<-t100[[1]] #select one tree for trimming purposes


t100<-lapply(t100,drop.tip,tip=setdiff(tree$tip.label,x$PhyloName)) #trim out everything from the tree that's not in the dataset

#t100 and x should now match


######
#prepare the data
######

x$zMass<-scale(x$Body.mass..log.)
x$zHWI<-scale(log(x$HWI))
x$zLat<-scale(abs(x$Latitude))
x$zTemp<-scale(x$AnnualTemp)
x$zTR<-scale(x$TempRange)
x$zPrecip<-scale(x$AnnualPrecip)
x$zPR<-scale(x$PrecipRange)

######
#set up a dummy run
######

i=1 #this is arbitrary

tree<-t100[[i]]  

animalA<-inverseA(tree)$Ainv 

#set up priors for this model
gelmanprior<-list(B=list(mu=rep(0,8), #mu has to be the number of parameters to be estimated (k-1 for each category of size k, 1 for each continuous variable, 1 for the intercept) 
                         V=gelman.prior(~zMass+zHWI+zLat+zTemp+zTR+zPrecip+zPR, # if you're changing this code, you'll have to adjust this equation to match your model, and mu above may have to change
                                        data = x,  scale=1+pi^2/3)), 
                  R=list(V=1,fix=1),G=list(G1=list(V=1E-10,nu=-1)))

mod<-MCMCglmm(as.factor(Loc.Artificial)~zMass+zHWI+zLat+zTemp+zTR+zPrecip+zPR,
              random=~PhyloName, 
              ginverse=list(PhyloName=animalA), 
              prior = gelmanprior, 
              verbose=TRUE, 
              family="categorical", 
              data = x,
              nitt=5500*2,
              thin=10,
              burnin=500*2,
              pl=TRUE, 
              pr=TRUE, 
              slice=TRUE) 

Final.disp<-mod #set up a structure that we'll populate with the real model
Final.disp$VCV[((i-1)*10+1):(i*10), ]<-mod$VCV[1:10,] 
Final.disp$Sol[((i-1)*10+1):(i*10), ]<-mod$Sol[1:10,] 
Final.disp$Liab[((i-1)*10+1):(i*10), ]<-mod$Liab[1:10,] 

nsamp.l<-nrow(mod$VCV)
start1.l=list(R=mod$VCV[nsamp.l,"units"], G=list(G1=mod$VCV[nsamp.l,"PhyloName"]))

save(Final.disp,file="Loc-Artificial.Rdata")

######
#run the full model
######
Sys.time() #just to time how long the loop takes

for(i in 1:100){ #loop through 100 trees
  tree<-t100[[i]] #select the ith tree 
  
  animalA<-inverseA(tree)$Ainv 
  
  mod<-MCMCglmm(as.factor(Loc.Artificial)~zMass+zHWI+zLat+zTemp+zTR+zPrecip+zPR,
                random=~PhyloName, 
                ginverse=list(PhyloName=animalA), 
                prior = gelmanprior, 
                verbose=FALSE, 
                family="categorical", 
                start= start1.l,
                data = x,
                nitt=20000, #this combo of nitt/thin/burnin will result in 10 samples / tree; if you're changing this code, this may have to be changed
                thin=1000,
                burnin=10000,
                pl=TRUE,
                pr=TRUE,
                slice=TRUE)
  
  print(i) #print which tree you're on (for your sanity as the loop runs)
  
  Final.disp$VCV[((i-1)*10+1):(i*10), ]<-mod$VCV[1:10,]  #put this tree's run into the overall structure
  Final.disp$Sol[((i-1)*10+1):(i*10), ]<-mod$Sol[1:10,] 
  Final.disp$Liab[((i-1)*10+1):(i*10), ]<-mod$Liab[1:10,] 
  
  nsamp.l<-nrow(mod$VCV)
  start1.l=list(R=mod$VCV[nsamp.l,"units"], G=list(G1=mod$VCV[nsamp.l,"PhyloName"]))
  
  save(Final.disp,file="Loc-Artificial.Rdata") #save, if it makes you feel better -- if you delete this, it'll speed up the code somewhat
  
}

save(Final.disp,file="Loc-Artificial.Rdata")
Sys.time()
