library(survsim)

NumCows = 200
Preg_rate = .8
dates = seq(as.Date("2016/5/1"), as.Date("2016/9/30"), "days") #Observation period
times = 1:length(dates)


#Cow survival parameters
lamb_cow = 0.001 #weibull scale parameter for adult cows summer hazard rate
p_cow = 1        #weibull shape parameter for constant hazard rate
S_cow = exp(-(lamb_cow*times)^p_cow)
plot(dates,S_cow, ylim = c(0,1))

#Calf survival parameters
lam_calf = .005
p_calf=.5                                 #weibull shape parameter for constant hazard rate
S_calf = exp(-(lam_calf*times)^p_calf)
plot(dates,S_calf, ylim = c(0,1))

#Paturition date parameters
mus <- c(30,52,74)        #Set mean (i.e., peak) birth date for each normal; 22 day cycle?
sds <- sqrt(c(9,9,9))     #Set std. dev. in birth date for each normal
props = c(.8,.15,.05)     #Set proportions for each peak

#Observation parameters (camera stuff)
CalfMeanAvail = 20    #Mean time (days) from birthday until a calf can be photographed
CalfStdAvail = 5     #Std deviation of normal dist. of avail times

PicProb = .2         #Probability an individual gets photographed each day

#End user input
################################################################################
Pop_t= data.frame(times)
Pop_t$NumCows = NA
Pop_t$NumCows[1] = NumCows
Pop_t$NumBorn = NA
Pop_t$NumCalves = 0
Pop_t$NumAvailCalves = 0
Pop_t$NumCalfPics = NA
Pop_t$NumCowPics = NA

end_time = length(dates)-1
times = 0:end_time
B_0_cow = -log(lamb_cow)
B_0_calf = -log(lam_calf)


#Simulate cow survival through observation period
cow_surv_times= simple.surv.sim(n= NumCows, foltime=end_time , dist.ev = c("weibull"), anc.ev=p_cow,beta0.ev=B_0_cow, dist.cens = c("unif"), anc.cens=end_time, beta0.cens=end_time)
hist(cow_surv_times$stop, freq = F, xlab = paste("time til death; censor at ",end_time))

#Generate prob. of birth date (mixture of 3 normal distributions)
prob_birthday_1 <- dnorm(x=times,mean=mus[1],sd=sds[1])
prob_birthday_2 <- dnorm(x=times,mean=mus[2],sd=sds[2])
prob_birthday_3 <- dnorm(x=times,mean=mus[3],sd=sds[3])
Prob_birthday = prob_birthday_1*props[1]+ prob_birthday_2*props[2]+prob_birthday_3*props[3]
ProbBorn_t = Preg_rate*Prob_birthday
plot(dates,Prob_birthday)

#Simulate calf survival and availability for picture capture
for (t in 1:length(times)){
    if (t>1){
          NumCowsDie = length(which(cow_surv_times$stop>=Pop_t$t[t-1]&cow_surv_times$stop<Pop_t$t[t]))
          Pop_t$NumCows[t] = Pop_t$NumCows[t-1]-NumCowsDie
    }
    Pop_t$NumBorn[t] = rbinom(n = 1, size = Pop_t$NumCows[t],prob = ProbBorn_t[t]) 
    if (Pop_t$NumBorn[t]>0){ 
      calf_surv_times= simple.surv.sim(n= Pop_t$NumBorn[t], foltime=(end_time-t) , dist.ev = c("weibull"), anc.ev=p_calf,beta0.ev=B_0_calf, dist.cens = c("unif"), anc.cens=(end_time-t), beta0.cens=(end_time-t))
      calf_surv_times
      calf_obs_StartTimes = t+round(rnorm(n = Pop_t$NumBorn[t], mean = CalfMeanAvail, sd= CalfStdAvail))
      
     for (row in 1:nrow(calf_surv_times)){
       Pop_t$NumCalves[t:(t+round(calf_surv_times$stop[row])+1)]=Pop_t$NumCalves[t:(t+round(calf_surv_times$stop[row])+1)]+1
       if(calf_surv_times$stop[row]>calf_obs_StartTimes[row]){
         Pop_t$NumAvailCalves[calf_obs_StartTimes[row]:(t+round(calf_surv_times$stop[row])+1)]= Pop_t$NumAvailCalves[calf_obs_StartTimes[row]:(t+round(calf_surv_times$stop[row])+1)]+1
       }     
     }
   }#end if 
}#end time loop
Pop_t$Calf_Cow = Pop_t$NumCalves/Pop_t$NumCows
plot(dates,Pop_t$NumAvailCalves)

#Take some freakin' picturs
for (t in 1:length(times)){
   Pop_t$NumCalfPics[t] = rbinom(n = 1, size = Pop_t$NumAvailCalves[t], prob = PicProb)
   Pop_t$NumCowPics[t] = rbinom(n = 1, size = Pop_t$NumCows[t], prob = PicProb)

}
plot(dates,Pop_t$NumCalfPics)


