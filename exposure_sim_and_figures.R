#--------------------#5  Comparison of estimated exposures for different types of care and room orientations ----------------------------------------------------------------------------------------------

#running function for left and right room orientation - IV care
behavior.sim(room.orientation = "left",caretype = "IV",numsequence = 1000, airsurf=TRUE)
seq.IV.left<-behavior.total
exposure.IV.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "IV",numsequence = 1000, airsurf=TRUE)
seq.IV.right<-behavior.total
exposure.IV.right<-exposure.frame

# running function for left and right room orientation - Obs care
behavior.sim(room.orientation = "left",caretype = "Obs",numsequence = 1000, airsurf=TRUE)
seq.Obs.left<-behavior.total
exposure.Obs.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Obs",numsequence = 1000, airsurf=TRUE)
seq.Obs.right<-behavior.total
exposure.Obs.right<-exposure.frame

# running function for left and right room orientation - Doctors' rounds
behavior.sim(room.orientation = "left",caretype = "Rounds",numsequence = 1000, airsurf=TRUE)
seq.Rounds.left<-behavior.total
exposure.Rounds.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Rounds",numsequence = 1000, airsurf=TRUE)
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
  scale_color_manual(name="Behaviour",
                     values=c("#99CCFF","#00CC99","#006666",
                        "#FF99FF","#0099CC","#0033FF",
                        "#9966FF","#99FFCC","#CCFF66"))+
  theme_pubr()+
  annotate("text",x=68,y=4e-5,label="Glove doffing",size=6)+
  annotate("text",x=30,y=1e-5,label="Glove donning",size=6)+
  annotate("segment", x = 70, xend = 65, y = 3.6e-5, yend = 3.6e-5, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = 31, xend = 28.5, y = 0.6e-5, yend = 0.1e-5, colour = "black", size=1, arrow=arrow())+
   theme(axis.text=element_text(size=20),axis.title=element_text(size=20),
        legend.text=element_text(size=20),legend.title=element_text(size=20),
        legend.box="vertical")


#extract summary statistics of exposures
hand.R.max<-rep(0,6000)
hand.R.mean<-rep(0,6000)
hand.L.max<-rep(0,6000)
hand.L.mean<-rep(0,6000)
hand.both.max<-rep(0,6000)
hand.both.mean<-rep(0,6000)
numcontacts<-rep(0,6000)
gloves<-rep(0,6000)
for (i in 1:6000){
  if(i<=1000){
    frame<-exposure.IV.left[[i]]
  }else if (i<=2000){
    frame<-exposure.IV.right[[i-1000]]
  }else if (i<=3000){
    frame<-exposure.Obs.left[[i-2000]]
  }else if (i<=4000){
    frame<-exposure.Obs.right[[i-3000]]
  }else if (i<=5000){
    frame<-exposure.Rounds.left[[i-4000]]
  }else{
    frame<-exposure.Rounds.right[[i-5000]]
  }
  
  #left and right hand grouped
  hand.both.max[i]<-max(c(frame$handR,frame$handL))
  hand.both.mean[i]<-mean(c(frame$handR,frame$handL))
  
  #right hand only
  hand.R.max[i]<-max(frame$handR)
  hand.R.mean[i]<-mean(frame$handR)
  
  #left hand only 
  hand.L.max[i]<-max(frame$handL)
  hand.L.mean[i]<-mean(frame$handL)
  
  # if (length(frame$behavior[frame$behavior=="GlovesOn"])>0){
  #    gloves[i]<-"yes"
  # }else{
  #  gloves[i]<-"no"
  # }
  
  numcontacts[i]<-length(frame$handR)
  
}

room.face<-rep(c(rep("Left-facing",1000),rep("Right-facing",1000)),3)
care<-c(rep("IV",2000),rep("Observation",2000),rep("Rounds",2000))
data<-data.frame(hand.R.max=hand.R.max,hand.both.max=hand.both.max,hand.both.mean=hand.both.mean,hand.R.mean=hand.R.mean,hand.L.max=hand.L.max,hand.L.mean=hand.L.mean,numcontacts=numcontacts,room.face=room.face,care=care,gloves=gloves)

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

# ------------------ trends over course of contacts over all

#one large data set with all results

for (i in 1:6000){
  
  if(i<=1000){
    if(i==1){
      total<-exposure.IV.left[[i]]
      total$care<-rep("IV",length(total$handR))
      total$orientation<-rep("left",length(total$handR))
      total$numcontact<-c(1:length(total$handR))
      total$run<-rep(i,length(total$handR))
      total$loss[1]<-NA
      if(length(total$behavior[total$behavior=="Alcohol" | total$behavior=="Gloves"])>0){
        total$alcohol<-rep("yes",length(total$behavior))
      }else{
        total$alcohol<-rep("no",length(total$behavior))
      }
      for (j in 2:length(total$behavior)){
        if(total$handR[j-1] > total$handR[j]){
          total$loss[j]<-"yes"
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
      if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
        frame$alcohol<-rep("yes",length(frame$behavior))
      }else{
        frame$alcohol<-rep("no",length(frame$behavior))
      }
      for (j in 2:length(frame$behavior)){
        if(frame$handR[j-1] > frame$handR[j]){
          frame$loss[j]<-"yes"
        }else{
          frame$loss[j]<-"no"
        }
      }
      frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    }
  }else if (i<=2000){
    frame<-exposure.IV.right[[i-1000]]
    frame$care<-rep("IV",length(frame$handR))
    frame$orientation<-rep("right",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else if (i<=3000){
    frame<-exposure.Obs.left[[i-2000]]
    frame$care<-rep("Obs",length(frame$handR))
    frame$orientation<-rep("left",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else if (i<=4000){
    frame<-exposure.Obs.right[[i-3000]]
    frame$care<-rep("Obs",length(frame$handR))
    frame$orientation<-rep("right",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else if (i<=5000){
    frame<-exposure.Rounds.left[[i-4000]]
    frame$care<-rep("Rounds",length(frame$handR))
    frame$orientation<-rep("left",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
      }else{
        frame$loss[j]<-"no"
      }
    }
    frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    
  }else{
    frame<-exposure.Rounds.right[[i-5000]]
    frame$care<-rep("Rounds",length(frame$handR))
    frame$orientation<-rep("right",length(frame$handR))
    frame$numcontact<-c(1:length(frame$handR))
    frame$run<-rep(i,length(frame$handR))
    frame$loss[1]<-NA
    if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="Gloves"])>0){
      frame$alcohol<-rep("yes",length(frame$behavior))
    }else{
      frame$alcohol<-rep("no",length(frame$behavior))
    }
    for (j in 2:length(frame$behavior)){
      if(frame$handR[j-1] > frame$handR[j]){
        frame$loss[j]<-"yes"
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
  }
}

#-----extract hand hygiene info-----------------
alcohol<-rep(NA,6000)
care<-rep(NA,6000)
room<-rep(NA,6000)

for(i in 1:6000){
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

mean.IV.right<-rep(NA,number.of.contacts)
sd.IV.right<-rep(NA,number.of.contacts)

mean.Obs.left<-rep(NA,number.of.contacts)
sd.Obs.left<-rep(NA,number.of.contacts)

mean.Obs.right<-rep(NA,number.of.contacts)
sd.Obs.right<-rep(NA,number.of.contacts)

mean.Rounds.left<-rep(NA,number.of.contacts)
sd.Rounds.left<-rep(NA,number.of.contacts)

mean.Rounds.right<-rep(NA,number.of.contacts)
sd.Rounds.right<-rep(NA,number.of.contacts)

for (i in 1:100){
  #------------ IV --------------------------------
  #IV - left facing
  mean.IV.left[i]<-mean(total$handR[total$care=="IV" & total$orientation=="left" & total$numcontact==i])
  sd.IV.left[i]<-sd(total$handR[total$care=="IV" & total$orientation=="left" & total$numcontact==i])
  
  #IV - right facing
  mean.IV.right[i]<-mean(total$handR[total$care=="IV" & total$orientation=="right" & total$numcontact==i])
  sd.IV.right[i]<-sd(total$handR[total$care=="IV" & total$orientation=="right" & total$numcontact==i])
  
  #------------ Obs --------------------------------
  #Obs - left facing
  mean.Obs.left[i]<-mean(total$handR[total$care=="Obs" & total$orientation=="left" & total$numcontact==i])
  sd.Obs.left[i]<-sd(total$handR[total$care=="Obs" & total$orientation=="left" & total$numcontact==i])
  
  #Obs - right facing
  mean.Obs.right[i]<-mean(total$handR[total$care=="Obs" & total$orientation=="right" & total$numcontact==i])
  sd.Obs.right[i]<-sd(total$handR[total$care=="Obs" & total$orientation=="right" & total$numcontact==i])
  
  #------------ Rounds --------------------------------
  #Rounds - left facing
  mean.Rounds.left[i]<-mean(total$handR[total$care=="Rounds" & total$orientation=="left" & total$numcontact==i])
  sd.Rounds.left[i]<-sd(total$handR[total$care=="Rounds" & total$orientation=="left" & total$numcontact==i])
  
  #Rounds - right facing
  mean.Rounds.right[i]<-mean(total$handR[total$care=="Rounds" & total$orientation=="right" & total$numcontact==i])
  sd.Rounds.right[i]<-sd(total$handR[total$care=="Rounds" & total$orientation=="right" & total$numcontact==i])
  
}

# combine into data.frame for geom_ribbon 

means<-c(mean.IV.left,mean.IV.right,mean.Obs.left,mean.Obs.right,mean.Rounds.left,mean.Rounds.right)
sd<-c(sd.IV.left,sd.IV.right,sd.Obs.left,sd.Obs.right,sd.Rounds.left,sd.Rounds.right)
care<-c(rep("IV",length(c(mean.IV.left,mean.IV.right))),
        rep("Obs",length(c(mean.Obs.left,mean.Obs.right))),
        rep("Rounds",length(c(mean.Rounds.left,mean.Rounds.right)))
)
numcount<-rep(c(1:length(mean.IV.left)),6)
facing<-rep(c(rep("Left-facing",length(mean.IV.left)),rep("Right-facing",length(mean.IV.right))),3)

data.all<-data.frame(means=means,sd=sd,care=care,facing=facing,numcount=numcount)

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

ggplot(data=bar.frame)+geom_bar(aes(y=percentage*100,x=care,fill=behavior),stat="identity")+facet_wrap(~orientation)+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
                        legend.text=element_text(size=15),legend.title=element_text(size=15),
                        legend.box="vertical",strip.text = element_text(size=15))+
  scale_y_continuous(name="Percentage (%)")+
  scale_x_discrete(name="")+
  scale_fill_manual(name="Behaviour",values=c("#99CCFF","#00CC99","#006666",
                                       "#FF99FF","#0099CC","#0033FF",
                                       "#9966FF","#99FFCC","#CCFF66"))
  

####################################################################################


# ANALYSIS OF MOMENTS WHEN CONC DECREASED

#
library(ggplot2)
ggplot(total[total$numcontact==2,])+geom_violin(aes(x=care,y=proploss,fill=orientation))+
  scale_y_continuous(name="Proportion of Contacts per Run that Resulted in Loss")+
  scale_fill_discrete(name="Orientation",labels=c("Left-facing","Right-facing"))+
  scale_x_discrete(name="Care Type",labels=c("IV","Observation","Doctors' Rounds"))+facet_wrap(~alcohol)

hist(total$numcontact[total$loss=="yes"])

hist(total$numcontact[total$loss=="no"])

ggplot(data=total[!is.na(total$loss),])+geom_density(aes(x=numcontact,fill=interaction(care,orientation)),alpha=0.5)+
  scale_x_continuous(trans="log10")+
  facet_wrap(~loss)

View(total[total$proploss>.50,])

ggplot(data=total[!is.na(total$loss),])+geom_point(aes(x=numcontact,y=behavior,colour=loss))+facet_wrap(orientation~care)

#--------------------------------------------------------------------------------------------------------------------------

in.maxyes<-rep(NA,max(total$numcontact)-1)
out.maxyes<-rep(NA,max(total$numcontact)-1)
patient.maxyes<-rep(NA,max(total$numcontact)-1)
hygieneinside.maxyes<-rep(NA,max(total$numcontact)-1)
equipment.maxyes<-rep(NA,max(total$numcontact)-1)
alcohol.maxyes<-rep(NA,max(total$numcontact)-1)
nearpatient.maxyes<-rep(NA,max(total$numcontact)-1)
farpatient.maxyes<-rep(NA,max(total$numcontact)-1)
gloveson.maxyes<-rep(NA,max(total$numcontact)-1)

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
    
    orientation<-rep(combo[[j]][2],max(total$numcontact))
    care<-rep(combo[[j]][1],max(total$numcontact))
    
    orientation.total<-c(orientation.total,orientation)
    care.total<-c(care.total,care)
    
  }
  
}

totalyes<-c(in.maxyes.total,out.maxyes.total,patient.maxyes.total,hygieneinside.maxyes.total,
            equipment.maxyes.total,alcohol.maxyes.total,nearpatient.maxyes.total,
            farpatient.maxyes.total,gloveson.maxyes.total)
length.all<-length(in.maxyes.total)
behavior<-c(rep("In",length.all),rep("Out",length.all),rep("Patient",length.all),rep("Hygiene Inside",length.all),
            rep("Equipment",length.all),rep("Alcohol",length.all),rep("Near Patient",length.all),rep("Far Patient",length.all),
            rep("Gloves",length.all))
numcount<-rep(1:max(total$numcontact),54)
care.total<-rep(care.total,9)
orientation.total<-rep(orientation.total,9)

frame.compare.yes.behavior<-data.frame(totalyes=totalyes,behavior=behavior,care.total=care.total,orientation.total=orientation.total,numcount=numcount)
frame.compare.yes.behavior$orientation.total[frame.compare.yes.behavior$orientation.total=="left"]<-"Left-facing"
frame.compare.yes.behavior$orientation.total[frame.compare.yes.behavior$orientation.total=="right"]<-"Right-facing"


#frame.compare.yes.behavior<-frame.compare.yes.behavior[frame.compare.yes.behavior$care.total=="IV",]
ggplot(data=frame.compare.yes.behavior[frame.compare.yes.behavior$numcount<=50 & frame.compare.yes.behavior$totalyes>0 & frame.compare.yes.behavior$numcount>1,],aes(numcount,behavior))+
  geom_tile(aes(fill=totalyes),colour="black")+
  scale_fill_gradient(low="light blue",high="black",name="Number of Decreases")+scale_x_continuous(name="Contact Number")+
  scale_y_discrete("Behaviour")+facet_wrap(orientation.total~care.total)+theme_pubr()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),
        legend.text=element_text(size=20),legend.title=element_text(size=20),
        legend.box="vertical",strip.text = element_text(size=20))+
  guides(fill=guide_colorbar(barwidth=10,barheight=1))





#histogram of lambda and lambda prior

prior<-runif(100000,0.0001,0.406)

transf<-c(lambda)
type<-c(rep("posterior",length(lambda)))

frame.te<-data.frame(trasnf=transf,type=type)

windows()
ggplot(frame.te)+geom_density(aes(x=transf),fill="light blue",alpha=0.3)+
  geom_histogram(aes(x=transf,y=..density..),fill="light blue",color="black",alpha=0.3)+
  scale_x_continuous(name="Posterior Transfer Efficiency")+
  scale_y_continuous(name="Density")+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))

#------------- other plots-------------------------

#ggplot(data=data,aes(x=numcontacts,y=hand.R.max,group=care))+geom_point(aes(x=numcontacts,y=hand.R.max,colour=care))+
#  geom_smooth(method='lm',aes(colour=care))+facet_wrap(~room.face)

A<-ggplot(data=data,aes(x=numcontacts,y=hand.R.max,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.max,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Max Conc. on Right Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

B<-ggplot(data=data,aes(x=numcontacts,y=hand.R.mean,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.mean,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Mean Conc. on Right Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

C<-ggplot(data=data,aes(x=numcontacts,y=hand.L.max,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.L.max,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Max Conc. on Left Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

D<-ggplot(data=data,aes(x=numcontacts,y=hand.L.mean,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.L.mean,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Mean Conc. on Left Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

library(ggpubr)
windows()
ggarrange(A,B,C,D,common.legend=TRUE)


E<-ggplot(data=data,aes(x=numcontacts,y=hand.both.max,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.max,colour=interaction(gloves,care)))+
  # geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+
  facet_wrap(~room.face)+
  scale_y_continuous(name="Max Conc. on Hands",trans="log10")+
  scale_x_continuous(name="Number of Contacts",trans="log10")+
  scale_color_discrete(name="Care Type")

G<-ggplot(data=data,aes(x=numcontacts,y=hand.both.mean,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.mean,colour=interaction(gloves,care)))+
  #geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+
  facet_wrap(~room.face)+
  scale_y_continuous(name="Mean Conc. on a Single Hand",trans="log10")+
  scale_x_continuous(name="Number of Contacts",trans="log10")+
  scale_color_discrete(name="Care Type",labels=c("IV, No Gloves","IV, Gloves","Observation, No Gloves",
                                                 "Observation, Gloves","Rounds, No Gloves"))

windows()
ggarrange(E,G,common.legend = TRUE,ncol=1)

windows()

ggplot(data=data)+geom_density(aes(x=hand.both.mean,fill=interaction(gloves,room.face)),alpha=.5)+facet_wrap(~care)+scale_x_continuous(trans="log10")
ggplot(data=data)+geom_density(aes(x=numcontacts,fill=interaction(gloves,room.face)),alpha=.5)+facet_wrap(~care)+scale_x_continuous(trans="log10")
ggplot(data=data[data$numcontacts==20,])+geom_density(aes(x=hand.both.mean,fill=interaction(gloves,room.face)),alpha=.5)+facet_wrap(~care)+scale_x_continuous(trans="log10")

G


