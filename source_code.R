
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if("readr" %in% rownames(installed.packages())==FALSE){install.packages("readr"); require(readr)}else{require(readr)}
detach("package:dplyr", unload = TRUE)
if("plyr" %in% rownames(installed.packages())==FALSE){install.packages("plyr"); require(plyr)}else{require(plyr)}
if("markovchain" %in% rownames(installed.packages())==FALSE){install.packages("markovchain"); require(markovchain)}else{require(markovchain)}
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}

#set up Markov chains
suppressMessages(suppressWarnings(source("markov_chain.R")))

#set up particle-tracking import
suppressMessages(suppressWarnings(source("particle_tracking_analysis.R")))

#set up exposure sim function
suppressMessages(suppressWarnings(source("sim_function.R")))

#run sims and analysis for figure/results generation
suppressMessages(suppressWarnings(source("exposure_sim_and_figures.R")))

data.all.airsurf<-data.all
data.all.airsurf$model<-"Deposition + Behavioural Differences"
total.airsurf<-total
total.airsurf$model<-"Deposition + Behavioural Differences"
total.airsurf<-subset(total.airsurf,select=-c(lossamount))

#run sims and analysis for figure/results generation
suppressMessages(suppressWarnings(source("exposure_sim_and_figures_airsurf_FALSE.R")))
data.all$model<-"Behavioural Differences Only"
total$model<-"Behavioural Differences Only"

data.all.combined<-rbind(data.all.airsurf,data.all)
total.all<-rbind(total.airsurf,total)

windows()
ggplot(data=data.all.combined)+
  geom_ribbon(aes(x=numcount,ymin=means-sd*1.96/sqrt(1000),ymax=means+sd*1.96/sqrt(1000),group=interaction(care,model),fill=care),alpha=0.5)+
  geom_line(aes(x=numcount,y=means,group=care,colour=care),linetype="twodash",size=1.5)+
  scale_y_continuous(name="Mean Concentration on Right Hand")+
  scale_x_continuous(name="Contact Number")+
  scale_color_manual(name="Care Type",values=c("#003333","#00CC99","#0099CC"))+
  scale_linetype_discrete(name="Care Type")+
  scale_fill_manual(name="Care Type",values=c("#003333","#00CC99","#0099CC"))+
  facet_wrap(model~facing,ncol=2)+theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))

suppressMessages(suppressWarnings(source("sensitivity_analysis.R")))


