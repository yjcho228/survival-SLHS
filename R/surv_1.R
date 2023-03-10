##########################
# T32 lecture series
# Init date: March 2023
# last update: March 9 2023
###################


#1. Survival plot for calendar time or observed time example
#2. Must compare two trt groups cal time vs abs time




# Load required packages


library(survival)
library(cmprsk)
library(SurvRegCensCov)
# Load example data
data(bladder2)


bladder2
rx1<-which(bladder2$rx==1) #1:106 placebo
rx2<-which(bladder2$rx==2)#107:178 thiotepa

dfbl2_1<-bladder2[rx1,]
dfbl2_2<-bladder2[rx2,]


ggplot(data=dfbl2_1,aes(stop,id))+
  geom_segment(aes(x=start,y=id,xend=stop,yend=id),colour="gray", linetype="solid")+
  geom_point(aes(colour=factor(enum),shape=factor(enum)))+
  scale_colour_discrete(name="Number of \nRecurrence",
                        breaks=c("1","2","3","4"),
                        labels=c("1","2","3","more than 4"))+
  scale_shape_discrete(name="Number of \nRecurrence",
                       breaks=c("1","2","3","4"),
                       labels=c("1","2","3","more than 4"))+
  theme(legend.justification=c(0.7,0.1),legend.position=c(0.7,0.1), 
        legend.background = element_rect(fill="gray90",size=0.5,linetype = "dotted"))+
  labs(title="Recurrent Time for Placebo group Bladder2 Data", y="Subject",x="Time until next recurrence")
#thiotepa

mydat<-dfbl2_1%>%mutate(startt=case_when(start==0~stop,
                                  start!=0~start)
                 )
ggplot(data=mydat,aes(stop,id))+
  geom_segment(aes(x=startt,y=id,xend=stop,yend=id),colour="gray", linetype="solid")+
  geom_point(aes(colour=factor(enum),shape=factor(enum)))+
  scale_colour_discrete(name="Number of \nRecurrence",
                        breaks=c("1","2","3","4"),
                        labels=c("1","2","3","more than 4"))+
  scale_shape_discrete(name="Number of \nRecurrence",
                       breaks=c("1","2","3","4"),
                       labels=c("1","2","3","more than 4"))+
  xlim(0,60)+
  theme(legend.justification=c(0.7,0.1),legend.position=c(0.7,0.1), 
        legend.background = element_rect(fill="gray90",size=0.5,linetype = "dotted"))+
  labs(title="Recurrent Time for Placebo group Bladder2 Data", y="Subject",x="Time until next recurrence")

# add time in unique id


mydatcal<-mydat%>%mutate(cal=stop-startt)

mydatacal<-data.frame(id=mydatcal[,"id"],cal=mydatcal[,"cal"])
# Define the data

# Define the function
fun <- function(id) {
  cumsum(mydatacal[which(mydatacal$id == id), "cal"])
}

# Initialize the output vector
d <- list(NA)
# Loop over the unique IDs
for (i in as.numeric(unique(mydatacal$id))) {
  
  d[[i]] <- fun(i)
  }

# Print the output
d

mydatcal$caltime<-unlist(d)


ggplot(data=mydatcal,aes(caltime,id))+
  geom_segment(aes(x=0,y=id,xend=caltime,yend=id),colour="gray", linetype="solid")+
  geom_point(aes(colour=factor(enum),shape=factor(enum)))+
  scale_colour_discrete(name="Number of \nRecurrence",
                        breaks=c("1","2","3","4"),
                        labels=c("1","2","3","more than 4"))+
  scale_shape_discrete(name="Number of \nRecurrence",
                       breaks=c("1","2","3","4"),
                       labels=c("1","2","3","more than 4"))+
  xlim(0,60)+
  theme(legend.justification=c(0.7,0.1),legend.position=c(0.7,0.1), 
        legend.background = element_rect(fill="gray90",size=0.5,linetype = "dotted"))+
  labs(title="Recurrent Time for Placebo group Bladder2 Data", y="Subject",x="Time until next recurrence")
