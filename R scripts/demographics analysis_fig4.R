data<-read.csv("1Mile_NATTS_demographics.csv")

popdata$minoritypct<-rbind(aggregate(data$MINORPCT, by=list(Location=data$Location), FUN=mean))
popdata$lowincpct<-rbind(aggregate(data$LOWINCPCT, by=list(Location=data$Location), FUN=mean))

write.csv(demdata,"demdata.csv") #aggregate to CR file

data<-read.csv("CRbyGroupNATTS5yrAvg.csv")

library(ggplot2)

reg_minor<-lm(minoritypercent~totalCRinAmil,data)
summary(reg_minor)

reg_lowinc<-lm(lowincpercent~totalCRinAmil,data)
summary(reg_lowinc)

pdf("lowincomepercent.pdf",useDingbats = FALSE)
ggplot(data,aes(x=totalCRinAmil,y=lowincpercent))+
  theme(axis.title.y = element_text(size=20),  axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),axis.line = element_line(size = 1),axis.title.x = element_text(size=20),panel.background = element_rect(fill="white"))+
  geom_point(size=4)+geom_smooth(method=lm,se=FALSE)
dev.off()

pdf("minoritypercent.pdf",useDingbats = FALSE)
ggplot(data,aes(x=totalCRinAmil,y=minoritypercent))+
  theme(axis.title.y = element_text(size=20),  axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),axis.line = element_line(size = 1),axis.title.x = element_text(size=20),panel.background = element_rect(fill="white"))+
  geom_point(size=4)+geom_smooth(method=lm,se=FALSE)
dev.off()
