
iter.total<-12000 #3 care types x 2 room orientations x 2 bioaerosol conditions

#inputs
mean.TE<-rep(NA,iter.total)
mean.SH<-rep(NA,iter.total)
mean.surf<-rep(NA,iter.total)
mean.efficacy<-rep(NA,iter.total)
count.farpatient<-rep(NA,iter.total)
count.nearpatient<-rep(NA,iter.total)
count.equipment<-rep(NA,iter.total)
count.patient<-rep(NA,iter.total)
count.hygieneinside<-rep(NA,iter.total)
count.gloves<-rep(NA,iter.total)
count.alcohol<-rep(NA,iter.total)

orientation<-rep(NA,iter.total)
care<-rep(NA,iter.total)

model<-rep(NA,iter.total)

#outputs
mean.hands<-rep(NA,iter.total)
max.hands<-rep(NA,iter.total)

for (i in 1:iter.total){
  
  if (i <=6000){
    #doing deposition + behavioural differences first
    frame.temp.1<-total.all[total.all$run==i & total.all$model=="Deposition + Behavioural Differences",]
    
  }else{
    #then behavioural differences only runs (adjust i for position in vector)
    frame.temp.1<-total.all[total.all$run==i-6000 & total.all$model=="Behavioural Differences Only",]
  }
  
  #inputs
  mean.TE[i]<-mean(frame.temp.1$transfer[!is.na(frame.temp.1$transfer)])
  mean.SH[i]<-mean(frame.temp.1$SH[!is.na(frame.temp.1$SH)])
  mean.surf[i]<-mean(frame.temp.1$surfconc[!is.na(frame.temp.1$surfconc)])
  mean.efficacy[i]<-mean(frame.temp.1$hygiene[!is.na(frame.temp.1$hygiene)])
  count.farpatient[i]<-length(frame.temp.1$behavior[frame.temp.1$behavior=="FarPatient"])
  count.nearpatient[i]<-length(frame.temp.1$behavior[frame.temp.1$behavior=="NearPatient"])
  count.equipment[i]<-length(frame.temp.1$behavior[frame.temp.1$behavior=="Equipment"])
  count.patient[i]<-length(frame.temp.1$behavior[frame.temp.1$behavior=="Patient"])
  count.hygieneinside[i]<-length(frame.temp.1$behavior[frame.temp.1$behavior=="HygieneInside"])
  count.gloves[i]<-length(frame.temp.1$behavior[frame.temp.1$behavior=="Gloves"])
  count.alcohol[i]<-length(frame.temp.1$behavior[frame.temp.1$behavior=="Alcohol"])
  
  orientation[i]<-frame.temp.1$orientation[1]
  care[i]<-frame.temp.1$care[1]
  
  model[i]<-frame.temp.1$model[1]
  
  #outputs
  mean.hands[i]<-mean(frame.temp.1$handR + frame.temp.1$handL)
  max.hands[i]<-max(frame.temp.1$handR + frame.temp.1$handL)
  
}

plot.frame<-data.frame(TE=mean.TE,SH=mean.SH,surf=mean.surf,eff=mean.efficacy,
                       count.farpatient=count.farpatient,count.nearpatient=count.nearpatient,
                       count.equipment=count.equipment,count.patient=count.patient,
                       count.hygieneinside=count.hygieneinside,count.gloves=count.gloves,
                       count.alcohol=count.alcohol,orientation=orientation,care=care,
                       model=model,meanhands=mean.hands,maxhands=max.hands)

#------- scatter plots ------------------------------------------------------
windows()
ggplot(plot.frame,aes(x=TE,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Transfer Efficiency")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=SH,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="FSA")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=log10(surf),y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name=expression("Log"[10]*phantom(x)*"Concentration on Surfaces"))+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=eff,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Hand Sanitiser Efficacy")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=count.farpatient,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Number of Far Patient Contacts")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=count.nearpatient,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Number of Near Patient Contacts")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=count.equipment,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Number of Equipment Contacts")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=count.patient,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Number of Patient Contacts")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")


ggplot(plot.frame,aes(x=count.hygieneinside,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Number of Hygiene Surface Contacts")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")

ggplot(plot.frame,aes(x=count.gloves,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Number of Glove Donning/Doffing Events")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")


ggplot(plot.frame,aes(x=count.alcohol,y=mean.hands,group=care,color=care))+geom_point(size=3,alpha=0.3)+
  #geom_smooth(method="loess",size=2,color="black")+
  theme_pubr()+
  scale_x_continuous(name="Number of Hand Sanitiser Events")+
  scale_linetype_discrete(name="Care Type")+
  scale_y_continuous(name="Mean Concentration on Hands")+
  scale_color_discrete(name="Care Type")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(~care,scales="free")


plot.frame$patient<-NA
plot.frame$patient[plot.frame$count.patient<1]<-"No patient contacts"
plot.frame$patient[plot.frame$count.patient>=1]<-"At least 1 patient contact"

windows()
ggplot(plot.frame,aes(x=orientation,y=log10(mean.hands),group=interaction(care,orientation,model,patient),fill=patient))+
  geom_violin(alpha=0.5,draw_quantiles = c(0.25,0.5,0.75))+
  theme_pubr()+
  scale_fill_discrete(name="")+
  scale_x_discrete(name="Room Orientation",labels=c("Left-facing","Right-facing"))+
  scale_y_continuous(name=expression("Log"[10]*phantom(x)*"Mean Conc. on Hands"))+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))+facet_wrap(model~care,scales="free")



#----------------------------------------------------------------------------

plot.frame.cor<-subset(plot.frame,select=c(-model,-orientation,-care,-eff))
cormat<-round(cor(plot.frame.cor,method="spearman"),2)
melted_cormat<-melt(cormat)

heatmaplabel<-c("Transfer Efficiency", "FSA", "Surface Conc",
                "Far Patient Contacts","Near Patient Contacts",
                "Equipment Contacts","Patient Contacts",
                "Hygiene Surface Contacts","Glove Donning/Doffing",
                "Hand Sanitiser Use","Mean Conc on Hands", "Max Conc on Hands")

ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+geom_text(aes(x=Var1,y=Var2,label=value),color="black",size=5)+
  scale_fill_gradient2(low="#33CCCC",mid="white",high="#FF9966",midpoint=0,name="Spearman Corr")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_discrete(name="",labels=heatmaplabel)+
  scale_y_discrete(name="",labels=heatmaplabel)

#-------left-facing----------------------------------------------------------

plot.frame.temp<-plot.frame[plot.frame$orientation=="left",]

plot.frame.temp.1<-plot.frame.temp[plot.frame.temp$care=="IV",]

plot.frame.cor<-subset(plot.frame.temp.1,select=c(-model,-orientation,-care,-eff,-patient))
cormat<-round(cor(plot.frame.cor,method="spearman"),2)
melted_cormat<-melt(cormat)

A<-ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+geom_text(aes(x=Var1,y=Var2,label=value),color="black",size=5)+
  scale_fill_gradient2(low="#33CCCC",mid="white",high="#FF9966",midpoint=0,name="Spearman Corr")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_discrete(name="",labels=heatmaplabel)+
  scale_y_discrete(name="",labels=heatmaplabel)+ggtitle("IV Care")

plot.frame.temp.2<-plot.frame.temp[plot.frame.temp$care=="Obs",]

plot.frame.cor<-subset(plot.frame.temp.2,select=c(-model,-orientation,-care,-eff,-patient))
cormat<-round(cor(plot.frame.cor,method="spearman"),2)
melted_cormat<-melt(cormat)

B<-ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+geom_text(aes(x=Var1,y=Var2,label=value),color="black",size=5)+
  scale_fill_gradient2(low="#33CCCC",mid="white",high="#FF9966",midpoint=0,name="Spearman Corr")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_discrete(name="",labels=heatmaplabel)+
  scale_y_discrete(name="",labels=heatmaplabel)+ggtitle("Observational Care")

plot.frame.temp.3<-plot.frame.temp[plot.frame.temp$care=="Rounds",]

plot.frame.cor<-subset(plot.frame.temp.3,select=c(-model,-orientation,-care,-eff,-patient))
cormat<-round(cor(plot.frame.cor,method="spearman"),2)
melted_cormat<-melt(cormat)

C<-ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+geom_text(aes(x=Var1,y=Var2,label=value),color="black",size=5)+
  scale_fill_gradient2(low="#33CCCC",mid="white",high="#FF9966",midpoint=0,name="Spearman Corr")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_discrete(name="",labels=heatmaplabel)+
  scale_y_discrete(name="",labels=heatmaplabel)+ggtitle("Doctor's Rounds")


windows()
A
windows()
B
windows()
C

#---------right-facing-----------------------------------------------

plot.frame.temp<-plot.frame[plot.frame$orientation=="right",]

plot.frame.temp.1<-plot.frame.temp[plot.frame.temp$care=="IV",]

plot.frame.cor<-subset(plot.frame.temp.1,select=c(-model,-orientation,-care,-eff,-patient))
cormat<-round(cor(plot.frame.cor,method="spearman"),2)
melted_cormat<-melt(cormat)

A<-ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+geom_text(aes(x=Var1,y=Var2,label=value),color="black",size=5)+
  scale_fill_gradient2(low="#33CCCC",mid="white",high="#FF9966",midpoint=0,name="Spearman Corr")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_discrete(name="",labels=heatmaplabel)+
  scale_y_discrete(name="",labels=heatmaplabel)+ggtitle("IV Care")

plot.frame.temp.2<-plot.frame.temp[plot.frame.temp$care=="Obs",]

plot.frame.cor<-subset(plot.frame.temp.2,select=c(-model,-orientation,-care,-eff,-patient))
cormat<-round(cor(plot.frame.cor,method="spearman"),2)
melted_cormat<-melt(cormat)

B<-ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+geom_text(aes(x=Var1,y=Var2,label=value),color="black",size=5)+
  scale_fill_gradient2(low="#33CCCC",mid="white",high="#FF9966",midpoint=0,name="Spearman Corr")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_discrete(name="",labels=heatmaplabel)+
  scale_y_discrete(name="",labels=heatmaplabel)+ggtitle("Observational Care")

plot.frame.temp.3<-plot.frame.temp[plot.frame.temp$care=="Rounds",]

plot.frame.cor<-subset(plot.frame.temp.3,select=c(-model,-orientation,-care,-eff,-patient))
cormat<-round(cor(plot.frame.cor,method="spearman"),2)
melted_cormat<-melt(cormat)

C<-ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+geom_text(aes(x=Var1,y=Var2,label=value),color="black",size=5)+
  scale_fill_gradient2(low="#33CCCC",mid="white",high="#FF9966",midpoint=0,name="Spearman Corr")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15),
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_discrete(name="",labels=heatmaplabel)+
  scale_y_discrete(name="",labels=heatmaplabel)+ggtitle("Doctor's Rounds")



windows()
A
windows()
B
windows()
C





