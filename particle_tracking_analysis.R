
#------------------------- #3 Reading in ANSYS results to make variable surface concentrations ---------------------------------------------------------------------------

ANSYS<-read.csv('C:/Users/wilso/Documents/Research/dissertation/preparation of chapters/Other materials/ANSYS St. James files/particle_comparison.csv')
ANSYS$SURFACEAREAm2<-ANSYS$SURFACEAREAm2*10000 #convert m2 to cm2
View(ANSYS)

#orientation 1 = right-facing, orientation 2 = left-facing
ANSYS$ORIENTATION[ANSYS$ORIENTATION==1]<-"Right-facing"
ANSYS$ORIENTATION[ANSYS$ORIENTATION==2]<-"Left-facing"

ANSYS$FRACTION<-(ANSYS$NUMBER.PARTICLE/ANSYS$TOTAL.PARTICLE)*100


A<-ggplot(data=ANSYS,aes(x=SURFACE,y=FRACTION,fill=SURFACE))+
  scale_y_continuous(name="Percent of Deposited Particles")+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
  geom_text(aes(label=round(FRACTION,2)), position=position_dodge(width=0.1), vjust=0.5,size=6)+
  coord_flip()+
  scale_fill_manual(values=c("#0099CC","#006666","#99FFCC","#00CC99","#9966FF","#99CCFF",
                               "#FF99FF"))+
  facet_wrap(~ORIENTATION)+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")+ggtitle("A")

B<-ggplot(data=ANSYS,aes(x=SURFACE,y=SURFACEAREAm2/10000,fill=SURFACE))+
  scale_y_continuous(name=expression("Surface Area (m"^2*")"))+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("#0099CC","#006666","#99FFCC","#00CC99","#9966FF","#99CCFF",
                             "#FF99FF"))+
  geom_text(aes(label=round(SURFACEAREAm2/10000,2)), position=position_dodge(width=0.1), vjust=0.5,size=6)+
  coord_flip()+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")+facet_wrap(~ORIENTATION)+ggtitle("B")

windows()
ggarrange(A,B,ncol=1)

