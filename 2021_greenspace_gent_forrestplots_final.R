
setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

gent92<-read.csv("20200820_meta_delta92_unadjusted.csv")
gent01<-read.csv("20200820_meta_delta01_unadjusted.csv")
gent92_adj<-read.csv("20200820_meta_delta92_adjusted.csv" )
gent01_adj<-read.csv("20200820_meta_delta01_adjusted.csv" )


################## FOREST PLOTS TO DISPLAY RESULTS###############
# PLOT OF JUST pop dens adjusted, restricted

library(ggplot2)
library(tidyr)
library(readxl)

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        #text=element_text(family='Times'),
        legend.title=element_blank())


################################
dp<-gent92
outcome<-as.data.frame(c("% NH Black", "% NH white", "% Hispanic",
                                                  "% Bachelors degree", "% Professional job",
                                                  "% Living in poverty", "Median household income",
                                                  "Median home value", "Median household rent"))
                           
                           
colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)
dp$q2_LL<-dp$greenp01_q2-(1.96*dp$greenp01_q2_se)
dp$q2_UL<-dp$greenp01_q2+(1.96*dp$greenp01_q2_se)
dp$q3_LL<-dp$greenp01_q3-(1.96*dp$greenp01_q3_se)
dp$q3_UL<-dp$greenp01_q3+(1.96*dp$greenp01_q3_se)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"
gathercols1 <-c("greenp01_q2", "greenp01_q3") 
gathercols2<-c("q2_LL", "q3_LL")
gathercols3<-c("q2_UL", "q3_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="greenp01_q2", "50th - 75th percentile", "> 75th percentile")

#do this to get the order correct in the figure
dpw$greenp_n<-factor(dpw$greenp_n, levels=c("50th - 75th percentile", "> 75th percentile"))
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                                "% Professional job", "% Bachelors degree", 
                                                "% NH Black", "% NH white", "% Hispanic",
                                                "Median home value", "Median household rent"))


setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200821_green1992_deltavars_unadjusted_gentrifiable.tiff", units="in", 
     width=12, height=7, res=600)

p = ggplot(data=dpw,
           aes(x= greenp_n,y = beta, ymin = LL, ymax = UL ))+
  geom_hline(yintercept =0, linetype=2)+
  geom_pointrange(aes(col=greenp_n))+
  #xlab('')+ ylab("Beta, 95% CI")+
  labs(x = "", y = "Beta (95% CI)", color="% Green, 1992") +
  geom_errorbar(aes(ymin=LL, ymax=UL,col=greenp_n),width=0.5,cex=1)+ 
  facet_wrap(~Outcome,strip.position="left",nrow=9,scales = "free_y")

p+theme_bw() +  theme(plot.title=element_text(size=16),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.text.x=element_text(face="bold"),
        #axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,
                                    angle=180))+
  coord_flip()

        
        


######### OLD PLOT vERSION
p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(Outcome), y = beta,
                           ymin =LL,
                           ymax = UL, color=greenp_n, shape=Outcome))
                           #color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "% Green space, 1992", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()

#### ADJusted 1992

dp<-gent92_adj
outcome<-as.data.frame(c("% NH Black", "% NH white", "% Hispanic",
                         "% Bachelors degree", "% Professional job",
                         "% Living in poverty", "Median household income",
                         "Median home value", "Median household rent"))


colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)
dp$q2_LL<-dp$greenp01_q2-(1.96*dp$greenp01_q2_se)
dp$q2_UL<-dp$greenp01_q2+(1.96*dp$greenp01_q2_se)
dp$q3_LL<-dp$greenp01_q3-(1.96*dp$greenp01_q3_se)
dp$q3_UL<-dp$greenp01_q3+(1.96*dp$greenp01_q3_se)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"
gathercols1 <-c("greenp01_q2", "greenp01_q3") 
gathercols2<-c("q2_LL", "q3_LL")
gathercols3<-c("q2_UL", "q3_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="greenp01_q2", "50th - 75th percentile", "> 75th percentile")

#do this to get the order correct in the figure
dpw$greenp_n<-factor(dpw$greenp_n, levels=c("50th - 75th percentile", "> 75th percentile"))
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                              "% Professional job", "% Bachelors degree", 
                                              "% NH Black", "% NH white", "% Hispanic",
                                              "Median home value", "Median household rent"))



setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200821_green1992_deltavars_adjusted_gentrifiable.tiff", units="in", 
     width=12, height=7, res=600)

p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(greenp_n), y = beta,
                           ymin =LL,
                           ymax = UL, color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "% Green space, 1992", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()

#2001 figures

dp<-gent01
outcome<-as.data.frame(c( "% NH Black",  "% NH white", "% Hispanic",
                          "% Bachelors degree","% Professional job", "% Living in poverty", 
                         "Median household income",
                         "Median home value", "Median household rent"))


colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)
dp$q3_LL<-dp$greenp01_q3-(1.96*dp$greenp01_q3_se)
dp$q3_UL<-dp$greenp01_q3+(1.96*dp$greenp01_q3_se)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"
gathercols1 <-c("greenp01_q3") 
gathercols2<-c("q3_LL")
gathercols3<-c("q3_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="greenp01_q3", "> 75th percentile",NA)

#LEVELS
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                              "% Professional job", "% Bachelors degree", 
                                              "% NH Black", "% NH white", "% Hispanic",
                                              "Median home value", "Median household rent"))



setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200821_green2001_deltavars_unadjusted_gentrifiable.tiff", units="in", 
     width=12, height=7, res=600)

p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(greenp_n), y = beta,
                           ymin =LL,
                           ymax = UL, color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "% Green space, 2001", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()

## 2001 adjusted


dp<-gent01_adj
outcome<-as.data.frame(c( "% NH Black",  "% NH white", "% Hispanic",
                          "% Bachelors degree","% Professional job", "% Living in poverty", 
                          "Median household income",
                          "Median home value", "Median household rent"))


colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)
dp$q3_LL<-dp$greenp01_q3-(1.96*dp$greenp01_q3_se)
dp$q3_UL<-dp$greenp01_q3+(1.96*dp$greenp01_q3_se)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"
gathercols1 <-c("greenp01_q3") 
gathercols2<-c("q3_LL")
gathercols3<-c("q3_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="greenp01_q3", "> 75th percentile",NA)
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                              "% Professional job", "% Bachelors degree", 
                                              "% NH Black", "% NH white", "% Hispanic",
                                              "Median home value", "Median household rent"))



setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200821_green2001_deltavars_adjusted_gentrifiable.tiff", units="in", 
     width=12, height=7, res=600)

p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(greenp_n), y = beta,
                           ymin =LL,
                           ymax = UL, color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "% Green space, 2001", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()

### CITY SPECIFIC PLOTS
# any gentrification plots


setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

s<-read.csv('20200908_green3cat_anygent_9000_adjusted_poissonrobust.csv')


setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis")
cbsa<-read_excel("cbsa_cityname_8_20_20.xlsx")

cbsa$CBSA<-as.factor(cbsa$CBSA)


s<-s[c("cbsa", "b2", "se2")]

s$CBSA<-as.numeric(s$cbsa)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "se2", "core.city")]

s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city

#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$RR<-exp(s_plot$b2)
s_plot$LL<-exp(s_plot$b2-(1.96*s_plot$se2))
s_plot$UL<-exp(s_plot$b2+(1.96*s_plot$se2))

data<-s_plot

min<-min(s_plot$LL) #0.20
max<-max(s_plot$UL) #7.33

#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=RR, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('RR, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=1, color='black', linetype='dashed')+
  apatheme

p

ggsave("v2_anygent_adj_citysp_forest.png", width = 20, height = 30, units = "cm", dpi=1600)

#######









setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis")
cbsa<-read_excel("cbsa_cityname_8_20_20.xlsx")

cbsa$CBSA<-as.factor(cbsa$CBSA)

#keep only delta college for now 

s<-s[which(s$linear_gent_var=="delta college ed 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city

#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p

ggsave("v2_cityspforest_deltacollege_unadj92.png", width = 20, height = 30,
       units = "cm", dpi=1600)


#####

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta blk 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city


#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p

ggsave("v2_cityspforest_deltablk_unadj92.png", width = 20, height = 30, units = "cm", dpi=1600)

########


s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta whit 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city

#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p

ggsave("v2_cityspforest_deltawht_unadj92.jpg", width = 20, height = 30, units = "cm", dpi=1600)

##########

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta median home value 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city


#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p
ggsave("v2_cityspforest_deltahomevalue_unadj92.png", width = 20, height = 30, units = "cm", dpi=1600)


################

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta household income 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city

#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p
ggsave("v2_cityspforest_deltaincome_unadj92.png", width = 20, height = 30, units = "cm", dpi=1600)

############

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta pov 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city
#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p
ggsave("v2_cityspforest_deltapoverty_unadj92.png", width = 20, height = 30, units = "cm", dpi=1600)


############

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta hispanic 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city

#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p
ggsave("v2_cityspforest_deltahispanic_unadj92.jpg", width = 20, height = 30, units = "cm", dpi=1600)

########

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta professional 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city

#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p

ggsave("v2_cityspforest_deltaprofesional_unadj92.png", width = 20, height = 30, units = "cm", dpi=1600)


###########

s<-read.csv('20200819_gent_green3cat92_cityspecific_spatial.csv')

s<-s[which(s$linear_gent_var=="delta median household rent 90-00"),]
# SELECT THE RELEVANT COLUMNS AND ROWS FROM RESULTS
colnames(s)<-c("CBSA", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

s$CBSA<-as.numeric(s$CBSA)
s$CBSA<-substr(s$CBSA,1,5)
s<-merge(s, cbsa, by="CBSA", all.x=T)

s_plot<-s[c("b2", "b2_se", "core.city")]

#s_plot$a_rank <- c(1 : dim(s_plot)[1])  # a vector of CBSA rank 
s_plot$city<-as.character(s_plot$core.city)
s_plot$name<- s_plot$city

#############

# SPECIFY ORDER
s_plot$name <- factor(s_plot$name, levels = s_plot$name[order(s_plot$b2)])

s_plot$LL<-s_plot$b2-(1.96*s_plot$b2_se)
s_plot$UL<-s_plot$b2+(1.96*s_plot$b2_se)

data<-s_plot

min<-min(s_plot$LL)
max<-max(s_plot$UL)
#Make a plot called 'p', and map citation data to y-axis, effect sizes to x-axis
#specify the min and max of the CIs, and give different shapes based on levels of tester
p=ggplot(data=data, aes(y=name, x=b2, xmin=LL, xmax=UL))+
  #Add data points and color them black
  geom_point(color = 'black')+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(min, max), ('Beta, 95% CI'))+
  #Give y-axis a meaningful label
  ylab('MSA')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  apatheme

p
ggsave("v2_cityspforest_deltarent_unadj92.png", width = 20, height = 30, units = "cm", dpi=1600)

#### EFFECT MODIFICATION FOREST PLOTS ##########

library(ggplot2)
library(tidyr)
setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

dp<-read.csv('20200820_meta_delta92_emm_blk.csv')

outcome<-as.data.frame(c("% NH Black", "% NH white", "% Hispanic",
                         "% Bachelors degree", "% Professional job",
                         "% Living in poverty", "Median household income",
                         "Median home value", "Median household rent"))


colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"

dp$white_LL<-dp$white-(1.96*dp$white_se)
dp$white_UL<-dp$white+(1.96*dp$white_se)


dp$black_LL<-dp$black-(1.96*dp$black_se)
dp$black_UL<-dp$black+(1.96*dp$black_se)

gathercols1 <-c("white", "black") 
gathercols2<-c("white_LL", "black_LL")
gathercols3<-c("white_UL", "black_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="white", "Low % NH-Black", "High % NH-Black")
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                              "% Professional job", "% Bachelors degree", 
                                              "% NH Black", "% NH white", "% Hispanic",
                                              "Median home value", "Median household rent"))

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200831_green1992_deltavars_unad_emmbyblk.tiff", units="in", 
     width=12, height=7, res=600)

p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(greenp_n), y = beta,
                           ymin =LL,
                           ymax = UL, color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()

### 2001####
setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")


dp<-read.csv('20200820_meta_delta01_emm_blk.csv')

outcome<-as.data.frame(c("% NH Black", "% NH white", "% Hispanic",
                         "% Bachelors degree", "% Professional job",
                         "% Living in poverty", "Median household income",
                         "Median home value", "Median household rent"))


colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"

dp$white_LL<-dp$white-(1.96*dp$white_se)
dp$white_UL<-dp$white+(1.96*dp$white_se)


dp$black_LL<-dp$black-(1.96*dp$black_se)
dp$black_UL<-dp$black+(1.96*dp$black_se)

gathercols1 <-c("white", "black") 
gathercols2<-c("white_LL", "black_LL")
gathercols3<-c("white_UL", "black_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="white", "Low % NH-Black", "High % NH-Black")
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                              "% Professional job", "% Bachelors degree", 
                                              "% NH Black", "% NH white", "% Hispanic",
                                              "Median home value", "Median household rent"))

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200831_green2001_deltavars_unad_emmbyblk.tiff", units="in", 
     width=12, height=7, res=600)

p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(greenp_n), y = beta,
                           ymin =LL,
                           ymax = UL, color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()

################################################################
setwd('C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results')
dp<-read.csv('20200820_meta_delta92_emm_hisp.csv')

outcome<-as.data.frame(c("% NH Black", "% NH white", "% Hispanic",
                         "% Bachelors degree", "% Professional job",
                         "% Living in poverty", "Median household income",
                         "Median home value", "Median household rent"))


colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"

dp$low_hisp_LL<-dp$low_hisp-(1.96*dp$low_hisp_se)
dp$low_hisp_UL<-dp$low_hisp+(1.96*dp$low_hisp_se)


dp$high_hisp_LL<-dp$high_hisp-(1.96*dp$high_hisp_se)
dp$high_hisp_UL<-dp$high_hisp+(1.96*dp$high_hisp_se)

gathercols1 <-c("low_hisp", "high_hisp") 
gathercols2<-c("low_hisp_LL", "high_hisp_LL")
gathercols3<-c("low_hisp_UL", "high_hisp_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="low_hisp", "Low % Hispanic", "High % Hispanic")
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                              "% Professional job", "% Bachelors degree", 
                                              "% NH Black", "% NH white", "% Hispanic",
                                              "Median home value", "Median household rent"))

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200831_green1992_deltavars_unad_emmbyhisp.tiff", units="in", 
     width=12, height=7, res=600)

p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(greenp_n), y = beta,
                           ymin =LL,
                           ymax = UL, color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()

### 2001####

setwd('C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results')

dp<-read.csv('20200820_meta_delta01_emm_hisp.csv')

outcome<-as.data.frame(c("% NH Black", "% NH white", "% Hispanic",
                         "% Bachelors degree", "% Professional job",
                         "% Living in poverty", "Median household income",
                         "Median home value", "Median household rent"))


colnames(outcome)<-"Outcome"
dp<-cbind(dp, outcome)

keycol <- "greenp"
valuecol1 <- "beta"
valuecol2<-"LL"
valuecol3<-"UL"

dp$low_hisp_LL<-dp$low_hisp-(1.96*dp$low_hisp_se)
dp$low_hisp_UL<-dp$low_hisp+(1.96*dp$low_hisp_se)


dp$high_hisp_LL<-dp$high_hisp-(1.96*dp$high_hisp_se)
dp$high_hisp_UL<-dp$high_hisp+(1.96*dp$high_hisp_se)

gathercols1 <-c("low_hisp", "high_hisp") 
gathercols2<-c("low_hisp_LL", "high_hisp_LL")
gathercols3<-c("low_hisp_UL", "high_hisp_UL")

dpw1<-gather_(dp, keycol, valuecol1, gathercols1)
dpw2<-gather_(dp, keycol, valuecol2, gathercols2)
dpw3<-gather_(dp, keycol, valuecol3, gathercols3)

dpw1<-dpw1[c("greenp", "Outcome", "beta")]
dpw2<-dpw2[c("greenp", "Outcome", "LL")]
dpw3<-dpw3[c("greenp", "Outcome", "UL")]

dpw<-cbind(dpw1, dpw2, dpw3)

dpw<-dpw[c(1, 2, 3, 6, 9)]

#(-0.001,0.25]    (0.25,0.5]    (0.5,0.75]      (0.75,1] 

dpw$greenp_n<-ifelse(dpw$greenp=="low_hisp", "Low % Hispanic", "High % Hispanic")
dpw$Outcome <- factor(dpw$Outcome, levels = c("Median household income", "% Living in poverty", 
                                              "% Professional job", "% Bachelors degree", 
                                              "% NH Black", "% NH white", "% Hispanic",
                                              "Median home value", "Median household rent"))

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results/Figures")

tiff("20200831_green2001_deltavars_unad_emmbyhisp.tiff", units="in", 
     width=12, height=7, res=600)

p <- ggplot( data = dpw,
             mapping = aes(x = as.factor(greenp_n), y = beta,
                           ymin =LL,
                           ymax = UL, color=Outcome, shape=Outcome))

q<-p + geom_pointrange(position = position_dodge(width = 0.60)) +
  
  labs(x = "", y = "Beta (95% CI)", 
       color="Change variable", shape="Change variable") 


r<-q + #+ scale_y_discrete(
  #limits=c(lim), expand = c(-0.2, 0.3)) +
  geom_hline(yintercept = 0, color="gray50", linetype=2) +
  # scale_size(range=c(2,5), guide=F)+
  scale_shape_manual(values=seq(0,8))

r+
  theme_bw() + 
  theme(text = element_text(size=12))

dev.off()


