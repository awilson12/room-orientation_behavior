
#------------------------- #3 Reading in ANSYS results to make variable surface concentrations ---------------------------------------------------------------------------

ANSYS<-read.csv('particle_comparison.csv')

View(ANSYS)

ANSYS$SURFACEAREAm2<-ANSYS$SURFACEAREAm2*10000 #convert m2 to cm2
View(ANSYS)

#orientation 1 = right-facing, orientation 2 = left-facing
ANSYS$ORIENTATION[ANSYS$ORIENTATION==1]<-"Right-facing"
ANSYS$ORIENTATION[ANSYS$ORIENTATION==2]<-"Left-facing"

#ANSYS$FRACTION<-(ANSYS$NUMBER.PARTICLE/ANSYS$TOTAL.PARTICLE)*100

ANSYStemp<-ANSYS[ANSYS$ACH==10,]
A<-ggplot(data=ANSYStemp,aes(x=SURFACE,y=FRACTION*100,fill=SURFACE,group=CONDITION))+
  scale_y_continuous(name="Percent of Deposited Particles",limits = c(0,85))+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
  geom_text(aes(label=round(FRACTION*100,2),group=interaction(CONDITION,ACH)), position=position_dodge(width=0.5), hjust=-0.1, vjust=0.5,size=4)+
  coord_flip()+
  scale_fill_manual(values=c("#0099CC","#006666","#99FFCC","#00CC99","#9966FF","#FF99FF","#99CCFF","#0099CC"))+
  facet_wrap(CONDITION~ORIENTATION)+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")
  #+ggtitle("A")

windows()
A

ANSYStemp2<-ANSYS[ANSYS$ACH==6,]
B<-ggplot(data=ANSYStemp2,aes(x=SURFACE,y=FRACTION*100,fill=SURFACE,group=CONDITION))+
  scale_y_continuous(name="Percent of Deposited Particles",limits = c(0,85))+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
  geom_text(aes(label=round(FRACTION*100,2),group=interaction(CONDITION,ACH)), position=position_dodge(width=0.5), hjust=-0.1, vjust=0.5,size=4)+
  coord_flip()+
  scale_fill_manual(values=c("#0099CC","#006666","#99FFCC","#00CC99","#9966FF","#FF99FF","#99CCFF","#0099CC"))+
  facet_wrap(CONDITION~ORIENTATION)+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")
#+ggtitle("A")

windows()
B

ANSYStemp3<-ANSYS[ANSYS$ACH==2.5,]
C<-ggplot(data=ANSYStemp3,aes(x=SURFACE,y=FRACTION*100,fill=SURFACE,group=CONDITION))+
  scale_y_continuous(name="Percent of Deposited Particles",limits = c(0,100))+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
  geom_text(aes(label=round(FRACTION*100,2),group=interaction(CONDITION,ACH)), position=position_dodge(width=0.5), hjust=-0.1, vjust=0.5,size=4)+
  coord_flip()+
  scale_fill_manual(values=c("#0099CC","#006666","#99FFCC","#00CC99","#9966FF","#FF99FF","#99CCFF","#0099CC"))+
  facet_wrap(CONDITION~ORIENTATION)+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")
#+ggtitle("A")

windows()
C

ANSYStemp4<-ANSYS[ANSYS$ACH==6 & ANSYS$CONDITION=="Window In, Door Out",]
D<-ggplot(data=ANSYStemp4,aes(x=SURFACE,y=FRACTION*100,fill=SURFACE,group=CONDITION))+
  scale_y_continuous(name="Percent of Deposited Particles",limits = c(0,85))+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
  geom_text(aes(label=round(FRACTION*100,2),group=interaction(CONDITION,ACH)), position=position_dodge(width=0.5), hjust=-0.1, vjust=0.5,size=6)+
  coord_flip()+
  scale_fill_manual(values=c("#0099CC","#006666","#99FFCC","#00CC99","#9966FF","#FF99FF","#99CCFF","#0099CC"))+
  facet_wrap(~ORIENTATION,ncol=1)+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")
#+ggtitle("A")

windows()
D
