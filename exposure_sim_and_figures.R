#--------------------#5  Comparison of estimated exposures for different types of care and room orientations ----------------------------------------------------------------------------------------------

#running function for left and right room orientation - IV care
behavior.sim(room.orientation = "left",caretype = "IV",numsequence = iter, airsurf=airsurfarg)
seq.IV.left<-behavior.total
exposure.IV.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "IV",numsequence = iter, airsurf=airsurfarg)
seq.IV.right<-behavior.total
exposure.IV.right<-exposure.frame

# running function for left and right room orientation - Obs care
behavior.sim(room.orientation = "left",caretype = "Obs",numsequence = iter, airsurf=airsurfarg)
seq.Obs.left<-behavior.total
exposure.Obs.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Obs",numsequence = iter, airsurf=airsurfarg)
seq.Obs.right<-behavior.total
exposure.Obs.right<-exposure.frame

# running function for left and right room orientation - Doctors' rounds
behavior.sim(room.orientation = "left",caretype = "Rounds",numsequence = iter, airsurf=airsurfarg)
seq.Rounds.left<-behavior.total
exposure.Rounds.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Rounds",numsequence = iter, airsurf=airsurfarg)
seq.Rounds.right<-behavior.total
exposure.Rounds.right<-exposure.frame

# example plots
person<-7
handconc<-c(exposure.IV.left[[person]]$handR,exposure.IV.left[[person]]$handL)
time<-c(rep(1:length(exposure.IV.left[[person]]$handR),2))
behavior<-rep(exposure.IV.left[[person]]$behavior,2)
hand<-c(rep("right",length(handconc)/2),rep("left",length(handconc)/2))
frame.1<-data.frame(handconc=handconc,behavior=behavior,hand=hand,time=time)

windows()
ggplot(data=frame.1,aes(x=time,y=handconc,group=hand))+
  geom_line(aes(linetype=hand),size=1.5)+geom_point(aes(colour=behavior),size=5)+
  scale_y_continuous(name=expression("Viral particles/cm"^2*"on hands"))+
  scale_x_continuous(name="Number of contacts")+
  scale_linetype(name="Hand")+
  scale_color_manual(name="Behavior",
                     values=c("#99CCFF","#00CC99","#006666",
                        "#FF99FF","#0099CC","#0033FF",
                        "#9966FF","#99FFCC","#CCFF66"))+
  theme_pubr()+
  annotate("text",x=68,y=1.1e-5,label="Glove doffing",size=6)+
  annotate("text",x=30,y=0.5e-5,label="Glove donning",size=6)+
  annotate("segment", x = 70, xend = 65, y = 0.9e-5, yend = 0.9e-5, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = 31, xend = 28.5, y = 0.4e-5, yend = 0.1e-5, colour = "black", size=1, arrow=arrow())+
   theme(axis.text=element_text(size=20),axis.title=element_text(size=20),
        legend.text=element_text(size=20),legend.title=element_text(size=20),
        legend.box="vertical")


#extract summary statistics of exposures
hand.R.max<-rep(0,6*iter)
hand.R.mean<-rep(0,6*iter)
hand.L.max<-rep(0,6*iter)
hand.L.mean<-rep(0,6*iter)
hand.both.max<-rep(0,6*iter)
hand.both.mean<-rep(0,6*iter)
numcontacts<-rep(0,6*iter)
infection<-rep(0,6*iter)
handhygiene<-rep(0,6*iter)
handhygienecount<-rep(0,6*iter)
patcontact<-rep(0,6*iter)
for (i in 1:(6*iter)){
  if(i<=iter){
    frame<-exposure.IV.left[[i]]
  }else if (i<=(2*iter)){
    frame<-exposure.IV.right[[i-iter]]
  }else if (i<=(3*iter)){
    frame<-exposure.Obs.left[[i-(2*iter)]]
  }else if (i<=(4*iter)){
    frame<-exposure.Obs.right[[i-(3*iter)]]
  }else if (i<=(5*iter)){
    frame<-exposure.Rounds.left[[i-(4*iter)]]
  }else{
    frame<-exposure.Rounds.right[[i-(5*iter)]]
  }
  
  #left and right hand grouped
  hand.both.max[i]<-max(c(frame$handR,frame$handL))
  hand.both.mean[i]<-mean(c(frame$handR,frame$handL))
  
  if(length(frame$handR[!is.na(frame$hygiene)])>0){
    handhygiene[i]<-"yes"
  }else{
    handhygiene[i]<-"no"
  }
  
  #right hand only
  hand.R.max[i]<-max(frame$handR)
  hand.R.mean[i]<-mean(frame$handR)
  
  #left hand only 
  hand.L.max[i]<-max(frame$handL)
  hand.L.mean[i]<-mean(frame$handL)
  
  infection[i]<-frame$infect[1]
  
  patcontact[i]<-length(frame$behavior[frame$behavior=="Patient"])
  
  numcontacts[i]<-length(frame$handR)
  
}

room.face<-rep(c(rep("Left-facing",iter),rep("Right-facing",iter)),3)
care<-c(rep("IV",2*iter),rep("Observation",2*iter),rep("Rounds",2*iter))
data<-data.frame(hand.R.max=hand.R.max,hand.both.max=hand.both.max,hand.both.mean=hand.both.mean,
                 hand.R.mean=hand.R.mean,hand.L.max=hand.L.max,hand.L.mean=hand.L.mean,
                 infection=infection,handhygiene=handhygiene,patcontact=patcontact,
                 numcontacts=numcontacts,room.face=room.face,care=care)


#summary stats for infection risk

doctor.left<-signif(mean(data$infection[data$care=="Rounds" & data$room.face=="Left-facing"]),2)
iv.left<-signif(mean(data$infection[data$care=="IV" & data$room.face=="Left-facing"]),2)
obs.left<-signif(mean(data$infection[data$care=="Observation" & data$room.face=="Left-facing"]),2)

#doctor.left2<-median(data$infection[data$care=="Rounds" & data$room.face=="Left-facing"])
#iv.left2<-median(data$infection[data$care=="IV" & data$room.face=="Left-facing"])
#obs.left2<-median(data$infection[data$care=="Observation" & data$room.face=="Left-facing"])

(doctor.left-iv.left)/iv.left*100

(doctor.left-obs.left)/obs.left*100

doctor.right<-signif(mean(data$infection[data$care=="Rounds" & data$room.face=="Right-facing"]),2)
iv.right<-signif(mean(data$infection[data$care=="IV" & data$room.face=="Right-facing"]),2)
obs.right<-signif(mean(data$infection[data$care=="Observation" & data$room.face=="Right-facing"]),2)

#doctor.right2<-median(data$infection[data$care=="Rounds" & data$room.face=="Right-facing"])
#iv.right2<-median(data$infection[data$care=="IV" & data$room.face=="Right-facing"])
#obs.right2<-median(data$infection[data$care=="Observation" & data$room.face=="Right-facing"])

(doctor.right-iv.right)/iv.right*100

(doctor.right-obs.right)/obs.right*100

#comparison between rooms
(doctor.right-doctor.left)/doctor.left*100
(iv.right-iv.left)/iv.left*100
(obs.left-obs.right)/obs.right*100


behavior.example1<-exposure.IV.left[[3]]
behavior.example1$numcontact<-c(1:length(behavior.example1$handR))

behavior.example2<-exposure.Obs.left[[3]]
behavior.example2$numcontact<-c(1:length(behavior.example2$handR))

behavior.example3<-exposure.Rounds.right[[13]]
behavior.example3$numcontact<-c(1:length(behavior.example3$handR))

merged<-rbind(behavior.example1,behavior.example2,behavior.example3)
merged$caretype<-c(rep("IV",length(behavior.example1$numcontact)),
                   rep("Observations",length(behavior.example2$numcontact)),
                   rep("Doctors' rounds",length(behavior.example3$numcontact)))
merged<-merged[order(merged$numcontact),]
merged$behavior<-as.character(merged$behavior)
merged$behavior[merged$behavior=="HygieneInside"]<-"Hygiene Surface"
merged$behavior[merged$behavior=="FarPatient"]<-"Far Patient Surface"
merged$behavior[merged$behavior=="Alcohol"]<-"Hand Hygiene"
merged$behavior[merged$behavior=="In"]<-"Entrance"
merged$behavior[merged$behavior=="Out"]<-"Exit"

data2<-data
data2$patcat<-"0 contacts"
data2$patcat[data2$patcontact>0]<-">0 contacts"

ggplot(data2)+
  geom_violin(aes(x=care,y=infection,group=interaction(care,room.face),fill=room.face),draw_quantiles = c(0.25,0.5,0.75))+
  scale_y_continuous(trans="log10")+
  facet_wrap(handhygiene~patcat)


# ------------------ trends over course of contacts over all

#one large data set with all results

for (i in 1:(6*iter)){
  
  if(i<=iter){
    if(i==1){
      total<-exposure.IV.left[[i]]
      total$care<-rep("IV",length(total$handR))
      total$orientation<-rep("left",length(total$handR))
      total$numcontact<-c(1:length(total$handR))
      total$run<-rep(i,length(total$handR))
      total$loss[1]<-NA
      total$lossamount[1]<-NA
      if(length(total$behavior[total$behavior=="Alcohol" | total$behavior=="Gloves"])>0){
        total$alcohol<-rep("yes",length(total$behavior))
      }else{
        total$alcohol<-rep("no",length(total$behavior))
      }
      for (j in 2:length(total$behavior)){
        if(total$handR[j-1] > total$handR[j]){
          total$loss[j]<-"yes"
          total$lossamount[j]<-total$handR[j-1]-total$handR[j]
        }else{
          total$loss[j]<-"no"
          
        }
      }
      total$proploss<-rep(length(total$loss[total$loss=="yes"])/length(total$behavior),length(total$behavior))
    }else{
      frame<-exposure.IV.left[[i]]
      frame$care<-rep("IV",length(frame$handR))
      frame$orientation<-rep("left",length(frame$handR))
      frame$numcontact<-c(1:length(frame$handR))
      frame$run<-rep(i,length(frame$handR))
      frame$loss[1]<-NA
      frame$lossamount[1]<-NA
      if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
        frame$alcohol<-rep("yes",length(frame$behavior))
      }else{
        frame$alcohol<-rep("no",length(frame$behavior))
      }
      for (j in 2:length(frame$behavior)){
        if(frame$handR[j-1] > frame$handR[j]){
          frame$loss[j]<-"yes"
          frame$lossamount[j]<-frame$handR[j-1]-frame$handR[j]
        }else{
          frame$loss[j]<-"no"
        }
      }
      frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    }
  }else if (i<=(2*iter)){
    frame<-exposure.IV.right[[i-iter]]
    frame$care<-rep("IV",length(frame$handR))
    frame$orientation<-rep("right",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    frame$lossamount[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
        frame$lossamount[j]<-frame$handR[j-1]-frame$handR[j]
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else if (i<=(3*iter)){
    frame<-exposure.Obs.left[[i-(2*iter)]]
    frame$care<-rep("Obs",length(frame$handR))
    frame$orientation<-rep("left",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    frame$lossamount[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
        frame$lossamount[j]<-frame$handR[j-1]-frame$handR[j]
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else if (i<=(4*iter)){
    frame<-exposure.Obs.right[[i-(3*iter)]]
    frame$care<-rep("Obs",length(frame$handR))
    frame$orientation<-rep("right",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    frame$lossamount[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
        frame$lossamount[j]<-frame$handR[j-1]-frame$handR[j]
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else if (i<=(5*iter)){
    frame<-exposure.Rounds.left[[i-(4*iter)]]
    frame$care<-rep("Rounds",length(frame$handR))
    frame$orientation<-rep("left",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    frame$lossamount[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
        frame$lossamount[j]<-frame$handR[j-1]-frame$handR[j]
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else{
    frame<-exposure.Rounds.right[[i-5*iter]]
    frame$care<-rep("Rounds",length(frame$handR))
    frame$orientation<-rep("right",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    frame$lossamount[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
        frame$lossamount[j]<-frame$handR[j-1]-frame$handR[j]
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
  }
  
  
  
  if(i==1){
    total<-total
  }else{
    total<-rbind(total,frame)
    
    if(i==2){
      #print(total)
    }
  }
  
  #print(summary(total$lossamount[!is.na(total$lossamount)]))
}

#-----extract hand hygiene info-----------------
alcohol<-rep(NA,6*iter)
care<-rep(NA,6*iter)
room<-rep(NA,6*iter)

for(i in 1:(6*iter)){
  alcohol[i]<-total$alcohol[total$run==i][1]
  care[i]<-total$care[total$run==i][1]
  room[i]<-total$orientation[total$run==i][1]
  
  if(room[i]=="left"){
    room[i]<-"Left-facing"
  }else{
    room[i]<-"Right-facing"
  }
  
}

frame.alcohol<-data.frame(alcohol=alcohol,care=care,room=room)

ggplot(frame.alcohol)+geom_bar(aes(fill=alcohol,x=care))+facet_wrap(~room)+
  scale_x_discrete(name="Care Type")+
  scale_y_continuous(name="Count")+
  scale_fill_discrete(name="Hand Hygiene Conducted?",labels=c("No","Yes"))+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))



### extract mean and sd for each contact number to do geom_ribbon later on
number.of.contacts<-100
mean.IV.left<-rep(NA,number.of.contacts)
sd.IV.left<-rep(NA,number.of.contacts)

num.sim.IV.left<-rep(NA,number.of.contacts)

mean.IV.right<-rep(NA,number.of.contacts)
sd.IV.right<-rep(NA,number.of.contacts)

num.sim.IV.right<-rep(NA,number.of.contacts)

mean.Obs.left<-rep(NA,number.of.contacts)
sd.Obs.left<-rep(NA,number.of.contacts)

num.sim.Obs.left<-rep(NA,number.of.contacts)

mean.Obs.right<-rep(NA,number.of.contacts)
sd.Obs.right<-rep(NA,number.of.contacts)

num.sim.Obs.right<-rep(NA,number.of.contacts)

mean.Rounds.left<-rep(NA,number.of.contacts)
sd.Rounds.left<-rep(NA,number.of.contacts)

num.sim.Rounds.left<-rep(NA,number.of.contacts)

mean.Rounds.right<-rep(NA,number.of.contacts)
sd.Rounds.right<-rep(NA,number.of.contacts)

num.sim.Rounds.right<-rep(NA,number.of.contacts)

for (i in 1:100){
  #------------ IV --------------------------------
  #IV - left facing
  mean.IV.left[i]<-mean(total$handR[total$care=="IV" & total$orientation=="left" & total$numcontact==i])
  sd.IV.left[i]<-sd(total$handR[total$care=="IV" & total$orientation=="left" & total$numcontact==i])
  
  num.sim.IV.left[i]<-length(total$handR[total$care=="IV" & total$orientation=="left" & total$numcontact==i])
  
  #IV - right facing
  mean.IV.right[i]<-mean(total$handR[total$care=="IV" & total$orientation=="right" & total$numcontact==i])
  sd.IV.right[i]<-sd(total$handR[total$care=="IV" & total$orientation=="right" & total$numcontact==i])
  
  num.sim.IV.right[i]<-length(total$handR[total$care=="IV" & total$orientation=="right" & total$numcontact==i])
  
  #------------ Obs --------------------------------
  #Obs - left facing
  mean.Obs.left[i]<-mean(total$handR[total$care=="Obs" & total$orientation=="left" & total$numcontact==i])
  sd.Obs.left[i]<-sd(total$handR[total$care=="Obs" & total$orientation=="left" & total$numcontact==i])
  
  num.sim.Obs.left[i]<-length(total$handR[total$care=="Obs" & total$orientation=="left" & total$numcontact==i])
  
  #Obs - right facing
  mean.Obs.right[i]<-mean(total$handR[total$care=="Obs" & total$orientation=="right" & total$numcontact==i])
  sd.Obs.right[i]<-sd(total$handR[total$care=="Obs" & total$orientation=="right" & total$numcontact==i])
  
  num.sim.Obs.right[i]<-length(total$handR[total$care=="Obs" & total$orientation=="right" & total$numcontact==i])
  
  #------------ Rounds --------------------------------
  #Rounds - left facing
  mean.Rounds.left[i]<-mean(total$handR[total$care=="Rounds" & total$orientation=="left" & total$numcontact==i])
  sd.Rounds.left[i]<-sd(total$handR[total$care=="Rounds" & total$orientation=="left" & total$numcontact==i])
  
  num.sim.Rounds.left[i]<-length(total$handR[total$care=="Rounds" & total$orientation=="left" & total$numcontact==i])
  
  #Rounds - right facing
  mean.Rounds.right[i]<-mean(total$handR[total$care=="Rounds" & total$orientation=="right" & total$numcontact==i])
  sd.Rounds.right[i]<-sd(total$handR[total$care=="Rounds" & total$orientation=="right" & total$numcontact==i])
  
  num.sim.Rounds.right[i]<-length(total$handR[total$care=="Rounds" & total$orientation=="right" & total$numcontact==i])
  
}

# combine into data.frame for geom_ribbon 

means<-c(mean.IV.left,mean.IV.right,mean.Obs.left,mean.Obs.right,mean.Rounds.left,mean.Rounds.right)
sd<-c(sd.IV.left,sd.IV.right,sd.Obs.left,sd.Obs.right,sd.Rounds.left,sd.Rounds.right)
care<-c(rep("IV",length(c(mean.IV.left,mean.IV.right))),
        rep("Obs",length(c(mean.Obs.left,mean.Obs.right))),
        rep("Rounds",length(c(mean.Rounds.left,mean.Rounds.right)))
        )
numsims<-c(num.sim.IV.left,num.sim.IV.right,num.sim.Obs.left,num.sim.Obs.right,num.sim.Rounds.left,num.sim.Rounds.right)
numcount<-rep(c(1:length(mean.IV.left)),6)
facing<-rep(c(rep("Left-facing",length(mean.IV.left)),rep("Right-facing",length(mean.IV.right))),3)

data.all<-data.frame(means=means,sd=sd,care=care,facing=facing,numcount=numcount,numsims)

# ---------- Calculate average breakdown of contacts per run 

prop.farpatient<-rep(0,6)
prop.nearpatient<-rep(0,6)
prop.equipment<-rep(0,6)
prop.hygiene<-rep(0,6)
prop.alcohol<-rep(0,6)
prop.gloves<-rep(0,6)
prop.in<-rep(0,6)
prop.out<-rep(0,6)
prop.patient<-rep(0,6)

combo<-list()
combo[[1]]<-c("IV","left")
combo[[2]]<-c("IV","right")
combo[[3]]<-c("Obs","left")
combo[[4]]<-c("Obs","right")
combo[[5]]<-c("Rounds","left")
combo[[6]]<-c("Rounds","right")


for (i in 1:6){
  combination<-combo[[i]]
  all.behaviors<-length(total$behavior[total$care==combination[1] & total$orientation==combination[2]])
  prop.farpatient[i]<-length(total$behavior[total$behavior=="FarPatient"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.nearpatient[i]<-length(total$behavior[total$behavior=="NearPatient"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.equipment[i]<-length(total$behavior[total$behavior=="Equipment"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.hygiene[i]<-length(total$behavior[total$behavior=="HygieneInside"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.alcohol[i]<-length(total$behavior[total$behavior=="Alcohol"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.gloves[i]<-length(total$behavior[total$behavior=="Gloves"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.in[i]<-length(total$behavior[total$behavior=="In"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.out[i]<-length(total$behavior[total$behavior=="Out"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  prop.patient[i]<-length(total$behavior[total$behavior=="Patient"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
}

percentage<-c(prop.farpatient,prop.nearpatient,prop.equipment,prop.hygiene,prop.alcohol,prop.gloves,prop.in,prop.out,prop.patient)
behavior<-c(rep("Far Patient",6),rep("Near Patient",6),rep("Equipment",6),rep("Hygiene",6),rep("Alcohol",6),rep("Gloves",6),rep("In",6),rep("Out",6),rep("Patient",6))
care<-c(rep(c("IV","IV","Obs","Obs","Rounds","Rounds"),9))
orientation<-rep(c("Left-facing","Right-facing","Left-facing","Right-facing","Left-facing","Right-facing"),9)
bar.frame<-data.frame(pecentage=percentage,behavior=behavior,care=care,orientation=orientation)

windows()
ggplot(data=bar.frame)+geom_bar(aes(y=percentage*100,x=care,fill=behavior),stat="identity")+facet_wrap(~orientation)+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
                        legend.text=element_text(size=15),legend.title=element_text(size=15),
                        legend.box="vertical",strip.text = element_text(size=15))+
  scale_y_continuous(name="Percent of Simulated Behaviors Comprising Care Episodes")+
  scale_x_discrete(name="")+
  scale_fill_manual(name="Behavior",values=c("#99CCFF","#00CC99","#006666",
                                       "#FF99FF","#0099CC","#0033FF",
                                       "#9966FF","#99FFCC","#CCFF66"))
  

####################################################################################


# ANALYSIS OF MOMENTS WHEN CONC DECREASED


in.maxyes<-rep(NA,max(total$numcontact)-1)
out.maxyes<-rep(NA,max(total$numcontact)-1)
patient.maxyes<-rep(NA,max(total$numcontact)-1)
hygieneinside.maxyes<-rep(NA,max(total$numcontact)-1)
equipment.maxyes<-rep(NA,max(total$numcontact)-1)
alcohol.maxyes<-rep(NA,max(total$numcontact)-1)
nearpatient.maxyes<-rep(NA,max(total$numcontact)-1)
farpatient.maxyes<-rep(NA,max(total$numcontact)-1)
gloveson.maxyes<-rep(NA,max(total$numcontact)-1)

in.meanchange<-rep(NA,max(total$numcontact)-1)
out.meanchange<-rep(NA,max(total$numcontact)-1)
patient.meanchange<-rep(NA,max(total$numcontact)-1)
hygieneinside.meanchange<-rep(NA,max(total$numcontact)-1)
equipment.meanchange<-rep(NA,max(total$numcontact)-1)
alcohol.meanchange<-rep(NA,max(total$numcontact)-1)
nearpatient.meanchange<-rep(NA,max(total$numcontact)-1)
farpatient.meanchange<-rep(NA,max(total$numcontact)-1)
gloves.meanchange<-rep(NA,max(total$numcontact)-1)

for(j in 1:6){
  for (i in 2:max(total$numcontact)){
    in.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="In" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    out.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Out" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    patient.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Patient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    hygieneinside.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="HygieneInside" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    equipment.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Equipment" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    alcohol.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Alcohol" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    nearpatient.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="NearPatient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    farpatient.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="FarPatient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    gloveson.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Gloves" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
  
    in.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="In" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    out.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="Out" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    patient.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="Patient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    hygieneinside.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="HygieneInside" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    equipment.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="Equipment" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    alcohol.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="Alcohol" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    nearpatient.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="NearPatient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    farpatient.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="FarPatient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    gloves.meanchange[i]<-mean(total$lossamount[total$loss=="yes" & total$behavior=="Gloves" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    
    
  }
  if(j==1){
    in.maxyes.total<-in.maxyes
    out.maxyes.total<-out.maxyes
    patient.maxyes.total<-patient.maxyes
    hygieneinside.maxyes.total<-hygieneinside.maxyes
    equipment.maxyes.total<-equipment.maxyes
    alcohol.maxyes.total<-alcohol.maxyes
    nearpatient.maxyes.total<-nearpatient.maxyes
    farpatient.maxyes.total<-farpatient.maxyes
    gloveson.maxyes.total<-gloveson.maxyes
    orientation.total<-rep(combo[[j]][2],max(total$numcontact))
    care.total<-rep(combo[[j]][1],max(total$numcontact))
    
    
    in.meanchange.total<-in.meanchange
    out.meanchange.total<-out.meanchange
    patient.meanchange.total<-patient.meanchange
    hygieneinside.meanchange.total<-hygieneinside.meanchange
    equipment.meanchange.total<-equipment.meanchange
    alcohol.meanchange.total<-alcohol.meanchange
    nearpatient.meanchange.total<-nearpatient.meanchange
    farpatient.meanchange.total<-farpatient.meanchange
    gloves.meanchange.total<-gloves.meanchange
    
  }else{
    in.maxyes.total<-c(in.maxyes.total,in.maxyes)
    out.maxyes.total<-c(out.maxyes.total,out.maxyes)
    patient.maxyes.total<-c(patient.maxyes.total,patient.maxyes)
    hygieneinside.maxyes.total<-c(hygieneinside.maxyes.total,hygieneinside.maxyes)
    equipment.maxyes.total<-c(equipment.maxyes.total,equipment.maxyes)
    alcohol.maxyes.total<-c(alcohol.maxyes.total,alcohol.maxyes)
    nearpatient.maxyes.total<-c(nearpatient.maxyes.total,nearpatient.maxyes)
    farpatient.maxyes.total<-c(farpatient.maxyes.total,farpatient.maxyes)
    gloveson.maxyes.total<-c(gloveson.maxyes.total,gloveson.maxyes)
    
    in.meanchange.total<-c(in.meanchange.total, in.meanchange)
    out.meanchange.total<-c(out.meanchange.total, out.meanchange)
    patient.meanchange.total<-c(patient.meanchange.total, patient.meanchange)
    hygieneinside.meanchange.total<-c(hygieneinside.meanchange.total, hygieneinside.meanchange)
    equipment.meanchange.total<-c(equipment.meanchange.total, equipment.meanchange)
    alcohol.meanchange.total<-c(alcohol.meanchange.total, alcohol.meanchange)
    nearpatient.meanchange.total<-c(nearpatient.meanchange.total,nearpatient.meanchange)
    farpatient.meanchange.total<-c(farpatient.meanchange.total, farpatient.meanchange)
    gloves.meanchange.total<-c(gloves.meanchange.total, gloves.meanchange)
    
    
    
    orientation<-rep(combo[[j]][2],max(total$numcontact))
    care<-rep(combo[[j]][1],max(total$numcontact))
    
    orientation.total<-c(orientation.total,orientation)
    care.total<-c(care.total,care)
    
  }
  
}

totalyes<-c(in.maxyes.total,out.maxyes.total,patient.maxyes.total,hygieneinside.maxyes.total,
            equipment.maxyes.total,alcohol.maxyes.total,nearpatient.maxyes.total,
            farpatient.maxyes.total,gloveson.maxyes.total)

totalchange<-c(in.meanchange.total,out.meanchange.total,patient.meanchange.total,hygieneinside.meanchange.total,
               equipment.meanchange.total,alcohol.meanchange.total,nearpatient.meanchange.total,
               farpatient.meanchange.total,gloves.meanchange.total)
length.all<-length(in.maxyes.total)
behavior<-c(rep("In",length.all),rep("Out",length.all),rep("Patient",length.all),rep("Hygiene Inside",length.all),
            rep("Equipment",length.all),rep("Alcohol",length.all),rep("Near Patient",length.all),rep("Far Patient",length.all),
            rep("Gloves",length.all))
numcount<-rep(1:max(total$numcontact),54)
care.total<-rep(care.total,9)
orientation.total<-rep(orientation.total,9)

frame.compare.yes.behavior<-data.frame(totalyes=totalyes,behavior=behavior,care.total=care.total,orientation.total=orientation.total,numcount=numcount,totalchange=totalchange)
frame.compare.yes.behavior$orientation.total[frame.compare.yes.behavior$orientation.total=="left"]<-"Left-facing"
frame.compare.yes.behavior$orientation.total[frame.compare.yes.behavior$orientation.total=="right"]<-"Right-facing"


#frame.compare.yes.behavior<-frame.compare.yes.behavior[frame.compare.yes.behavior$care.total=="IV",]
windows()
ggplot(data=frame.compare.yes.behavior[frame.compare.yes.behavior$numcount<=50 & frame.compare.yes.behavior$totalyes>0 & frame.compare.yes.behavior$numcount>1,],aes(numcount,behavior))+
  geom_tile(aes(fill=totalyes),colour="black")+
  scale_fill_gradient(low="light blue",high="black",name="Number of Decreases")+scale_x_continuous(name="Contact Number")+
  scale_y_discrete("Behavior")+facet_wrap(orientation.total~care.total)+theme_pubr()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),
        legend.text=element_text(size=20),legend.title=element_text(size=20),
        legend.box="vertical",strip.text = element_text(size=20),title=element_text(size=20))+
  guides(fill=guide_colorbar(barwidth=20,barheight=1))+ggtitle("A")

windows()
frame.compare.temp<-frame.compare.yes.behavior[!is.na(frame.compare.yes.behavior$totalchange),]
ggplot(data=frame.compare.temp[frame.compare.temp$numcount<=50 & frame.compare.temp$totalyes>0 & frame.compare.temp$numcount>1,],aes(numcount,behavior))+
  geom_tile(aes(fill=log10(totalchange)),colour="black")+
  scale_fill_gradient2(low="white",mid="light blue",high="black",midpoint=-7,name=expression("log"[10]*phantom(x)*"Mean Change in Concentration"))+scale_x_continuous(name="Contact Number")+
  scale_y_discrete("Behavior")+facet_wrap(orientation.total~care.total)+theme_pubr()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),
        legend.text=element_text(size=20),legend.title=element_text(size=20),
        legend.box="vertical",strip.text = element_text(size=20),title=element_text(size=20))+
  guides(fill=guide_colorbar(barwidth=20,barheight=1))+ggtitle("B")

#windows()
#frame.compare.temp<-frame.compare.yes.behavior[!is.na(frame.compare.yes.behavior$totalchange),]
#ggplot(data=frame.compare.temp[frame.compare.temp$numcount<=50 & frame.compare.temp$totalyes>0 & frame.compare.temp$numcount>1,],aes(numcount,behavior))+
#  geom_tile(aes(fill=totalchange),colour="black")+
#  scale_fill_gradient2(low="white",mid="light blue",high="black",name="Mean Change in Concentration")+scale_x_continuous(name="Contact Number")+
#  scale_y_discrete("Behaviour")+facet_wrap(orientation.total~care.total)+theme_pubr()+
#  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),
#        legend.text=element_text(size=20),legend.title=element_text(size=20),
#        legend.box="vertical",strip.text = element_text(size=20),title=element_text(size=20))+
#  guides(fill=guide_colorbar(barwidth=20,barheight=1))+ggtitle("B")

#exploring simulations with greatest loss moments
frame.sorted<-total[order(-total$lossamount),]
frame.greatest.loss<-frame.sorted[1:10,]
run.label<-c("Sim 1", "Sim 2", "Sim 3","Sim 4","Sim 5",
             "Sim 6", "Sim 7","Sim 8", "Sim 9", "Sim 10")

for (i in 1:10){
  if (i==1){
    frame.store<-total[total$run==frame.greatest.loss$run[i],]
    frame.store$run<-run.label[i]
  }else{
    frame.temp<-total[total$run==frame.greatest.loss$run[i],]
    frame.temp$run<-run.label[i]
    frame.store<-rbind(frame.store,frame.temp)
  }
}

frame.store$run<-factor(frame.store$run,labels=run.label)

#View(frame.store)
windows()
ggplot(frame.store)+geom_point(aes(x=numcontact,y=handR,color=behavior,group=run),size=4)+
  geom_line(aes(x=numcontact,y=handR,group=run))+facet_wrap(~run,scales="free")+
  scale_color_discrete(name="Behavior")+
  scale_x_continuous((name="Contact Number"))+
  scale_y_continuous(name=expression("Viral particles/cm"^2*"on hands"))+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=20),
        legend.text=element_text(size=20),legend.title=element_text(size=20),
        legend.box="vertical",strip.text = element_text(size=20))


                                      



