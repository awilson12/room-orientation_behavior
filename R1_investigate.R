#exploring frequency of hand hygiene and frequency of the glove donning/doffing sequence issue to
#address reviewer comments


airsurfarg=TRUE

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
numevents<-rep(0,6*iter)
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
  
  numevents[i]<-length(frame$handR)
  
  #left and right hand grouped
  hand.both.max[i]<-max(c(frame$handR,frame$handL))
  hand.both.mean[i]<-mean(c(frame$handR,frame$handL))
  
  if(length(frame$handR[!is.na(frame$hygiene)])>0){
    handhygiene[i]<-"yes"
  }else{
    handhygiene[i]<-"no"
  }
  
  handhygienecount[i]<-length(frame$handR[!is.na(frame$hygiene)])
  
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
                 infection=infection,handhygiene=handhygiene,handhygienecount=handhygienecount,
                 patcontact=patcontact,numevents=numevents,
                 numcontacts=numcontacts,room.face=room.face,care=care)

require(ggpmisc)

windows()
ggplot(data[data$handhygienecount>0,])+
  geom_point(aes(x=handhygienecount/numevents*100,y=hand.R.mean,color=room.face),size=4,alpha=0.5)+
  scale_x_continuous(name="Percent of Events Comprising Hand Hygiene",trans="log10",
                     labels = scales::number_format(accuracy = 1))+
  scale_y_continuous(name="Mean Concentration on Right Hand",trans="log10")+
  stat_cor(method = "spearman", label.x = 0,label.y=-3,size=5,aes(x=handhygienecount/numevents*100,y=hand.R.mean))+
  #geom_smooth(aes(x=handhygienecount/numevents*100,y=hand.R.mean),method="lm")+
  stat_smooth(method = "lm", aes(x=handhygienecount/numevents*100,y=hand.R.mean),color="black")+
  scale_color_manual(name="",values=c("#99FFCC","#99CCFF"))+
  facet_wrap(room.face~care)+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
             legend.text=element_text(size=15),legend.title=element_text(size=15),
             legend.box="vertical",strip.text = element_text(size=15))

#left-facing---------------------------------------------------------------------------------------------
data.cor<-data[data$care=="Observation" & data$room.face=="Left-facing" & data$handhygienecount>0,]
cor.test(log10(data.cor$hand.R.mean),log10(data.cor$handhygienecount/data.cor$numevents*100),method="spearman")

data.cor<-data[data$care=="Rounds" & data$room.face=="Left-facing" & data$handhygienecount>0,]
cor.test(log10(data.cor$hand.R.mean),log10(data.cor$handhygienecount/data.cor$numevents*100),method="spearman")

data.cor<-data[data$care=="IV" & data$room.face=="Left-facing" & data$handhygienecount>0,]
cor.test(log10(data.cor$hand.R.mean),log10(data.cor$handhygienecount/data.cor$numevents*100),method="spearman")

#right-facing----------------------------------------------------------------------------------------------
data.cor<-data[data$care=="Observation" & data$room.face=="Right-facing" & data$handhygienecount>0,]
cor.test(log10(data.cor$hand.R.mean),log10(data.cor$handhygienecount/data.cor$numevents*100),method="spearman")

data.cor<-data[data$care=="Rounds" & data$room.face=="Right-facing" & data$handhygienecount>0,]
cor.test(log10(data.cor$hand.R.mean),log10(data.cor$handhygienecount/data.cor$numevents*100),method="spearman")

data.cor<-data[data$care=="IV" & data$room.face=="Right-facing" & data$handhygienecount>0,]
cor.test(log10(data.cor$hand.R.mean),log10(data.cor$handhygienecount/data.cor$numevents*100),method="spearman")

#------------------------ Looking into glove doffing/donning issue - how often are gloves doffed when not donned or donned
#------------------------ when already donned?

source('sim_function_v2_PPEcheck.R')

#running function for left and right room orientation - IV care
behavior.sim2(room.orientation = "left",caretype = "IV",numsequence = iter, airsurf=airsurfarg)
gloveissuetotal.LIV<-gloveissuetotal
numyes.LIV<-length(gloveissuetotal.LIV[!is.na(gloveissuetotal.LIV)])
percentyes.LIV<-numyes.LIV/length(gloveissuetotal.LIV)*100

behavior.sim2(room.orientation = "right",caretype = "IV",numsequence = iter, airsurf=airsurfarg)
gloveissuetotal.RIV<-gloveissuetotal
numyes.RIV<-length(gloveissuetotal.RIV[!is.na(gloveissuetotal.RIV)])
percentyes.RIV<-numyes.RIV/length(gloveissuetotal.RIV)*100

# running function for left and right room orientation - Obs care
behavior.sim2(room.orientation = "left",caretype = "Obs",numsequence = iter, airsurf=airsurfarg)
gloveissuetotal.LObs<-gloveissuetotal
numyes.LObs<-length(gloveissuetotal.LObs[!is.na(gloveissuetotal.LObs)])
percentyes.LObs<-numyes.LObs/length(gloveissuetotal.LObs)*100

behavior.sim2(room.orientation = "right",caretype = "Obs",numsequence = iter, airsurf=airsurfarg)
gloveissuetotal.RObs<-gloveissuetotal
numyes.RObs<-length(gloveissuetotal.RObs[!is.na(gloveissuetotal.RObs)])
percentyes.RObs<-numyes.RObs/length(gloveissuetotal.RObs)*100

# running function for left and right room orientation - Doctors' rounds
behavior.sim2(room.orientation = "left",caretype = "Rounds",numsequence = iter, airsurf=airsurfarg)
gloveissuetotal.LRounds<-gloveissuetotal
numyes.LRounds<-length(gloveissuetotal.LRounds[!is.na(gloveissuetotal.LRounds)])
percentyes.LRounds<-numyes.LRounds/length(gloveissuetotal.LRounds)*100

behavior.sim2(room.orientation = "right",caretype = "Rounds",numsequence = iter, airsurf=airsurfarg)
gloveissuetotal.RRounds<-gloveissuetotal
numyes.RRounds<-length(gloveissuetotal.RRounds[!is.na(gloveissuetotal.RRounds)])
percentyes.RRounds<-numyes.RRounds/length(gloveissuetotal.RRounds)*100


#left-facing rooms
percentyes.LIV
percentyes.LRounds
percentyes.LObs

#right-facing rooms
percentyes.RIV
percentyes.RRounds
percentyes.RObs



