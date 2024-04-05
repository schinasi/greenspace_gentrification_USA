
library(haven)
library(readr)
library(gtools)
library(tableone)
library(metafor)
library(rgdal)
library(spdep)

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/ReCVD Data")

load('green_gent_analysis_20200609.Rda') #59631 #130 variables

# subset to only MSAS with 300+ census tracts

count<-as.data.frame(table(mpg$m10_cen_uid_u_2010))

how_many<-merge(mpg, count, by.x="m10_cen_uid_u_2010", by.y="Var1", all.x=T) 


df<-subset(how_many, how_many$Freq>300) #36338 


# Descriptive stats on the entire data set


library(raster)
usa <- shapefile("C:/Users/lhs36/Desktop/usa_censustracts/usa_censustracts.shp") #73682

# PREPARE FOR MERGE> HAVE TO CREATE FIPS VARIABLE, and PAD with Zeros to make comparoable to what's in the shapefile

df$FIPS<-as.character(df$t10_cen_uid_u_2010) #36338

library(stringr)
df$FIPS<-str_pad(df$FIPS, width=11, side = c("left"), pad = "0")

dfs<-merge(usa, df, by="FIPS", all.y=T) #73682
dfs<-subset(dfs, dfs$Freq>300) #36337


# SCALE THE CONTINUOUS vARIABLES
dfs$s_deltablk9000<-scale(dfs$deltablk9000)
dfs$s_deltablk0010<-scale(dfs$deltablk0010)
dfs$s_deltawht9000<-scale(dfs$deltawht9000)
dfs$s_deltawht0010<-scale(dfs$deltawht0010)
dfs$s_deltapov9000<-scale(dfs$t10_ldb_npov_p_2000-dfs$t10_ldb_npov_p_1990)
dfs$s_deltapov1000<-scale(dfs$t10_ldb_npov_p_2012-dfs$t10_ldb_npov_p_2000)
dfs$s_delta_col_p9000<-scale(dfs$t10_ldb_col_p_2000-dfs$t10_ldb_col_p_1990)
dfs$s_delta_col_p0010<-scale(dfs$t10_ldb_col_p_2012-dfs$t10_ldb_col_p_2000)
dfs$s_delta_hinci9000<-scale(dfs$t10_ldb_hinci_m_2000.x-dfs$t10_ldb_hinci_m_1990.x)
dfs$s_delta_hinci0010<-scale(dfs$t10_ldb_hinci_m_2012.x-dfs$t10_ldb_hinci_m_2000.x)
dfs$s_delta_mrenti9000<-scale(dfs$t10_ldb_mrenti_m_2000-dfs$t10_ldb_mrenti_m_1990)
dfs$s_delta_mrenti0010<-scale(dfs$t10_ldb_mrenti_m_2012-dfs$t10_ldb_mrenti_m_2000)

#inflation adjusted!!
dfs$s_delta_mhmvali9000<-scale(dfs$t10_ldb_mhmvali_m_2000-dfs$t10_ldb_mhmvali_m_1990)
dfs$s_delta_mhmvali0010<-scale(dfs$t10_ldb_mhmvali_m_2012-dfs$t10_ldb_mhmvali_m_2000)

dfs$s_deltahisp9000<-scale(dfs$t10_ldb_hisp_p_2000-dfs$t10_ldb_hisp_p_1990)
dfs$s_deltahisp0010<-scale(dfs$t10_ldb_hisp_p_2012-dfs$t10_ldb_hisp_p_2000)

dfs$deltahisp9000<-(dfs$t10_ldb_hisp_p_2000-dfs$t10_ldb_hisp_p_1990)
dfs$deltahisp0010<-(dfs$t10_ldb_hisp_p_2012-dfs$t10_ldb_hisp_p_2000)

dfs$s_deltaprof9000<-scale(dfs$t10_ldb_prof_p_2000-dfs$t10_ldb_prof_p_1990)
dfs$s_deltaprof0010<-scale(dfs$t10_ldb_prof_p_2012-dfs$t10_ldb_prof_p_2000)


dfs$deltaprof9000<-(dfs$t10_ldb_prof_p_2000-dfs$t10_ldb_prof_p_1990)
dfs$deltaprof0010<-(dfs$t10_ldb_prof_p_2012-dfs$t10_ldb_prof_p_2000)

# categorical delta variables

var<-dfs$deltablk0010
dfs$deltablk4cat<-cut(var, breaks=3)

var<-dfs$deltawht0010
dfs$deltawht4cat<-cut(var, breaks=4)

dfs$deltapov0010<-(dfs$t10_ldb_npov_p_2012-dfs$t10_ldb_npov_p_2000)
var<-dfs$deltapov0010
dfs$deltapov4cat<-cut(var, breaks=4)

dfs$deltacolp0010<-(dfs$t10_ldb_col_p_2012-dfs$t10_ldb_col_p_2000)
var<-dfs$deltacolp0010
dfs$deltacol4cat<-cut(var, breaks=4)

dfs$deltahinci0010<-(dfs$t10_ldb_hinci_m_2012.x-dfs$t10_ldb_hinci_m_2000.x)
var<-dfs$deltahinci0010
dfs$deltacol4cat<-cut(var, breaks=4)

dfs$deltamrenti0010<-(dfs$t10_ldb_mrenti_m_2012-dfs$t10_ldb_mrenti_m_2000)
var<-dfs$deltamrenti0010
p4=quantile(var, p=c(0, 0.25, 0.50, 0.75), na.rm=T)
dfs$deltamrent4cat<-cut(var, breaks=4)

dfs$delta_mhmvali0010<-(dfs$t10_ldb_mhmvali_m_2012-dfs$t10_ldb_mhmvali_m_2000)
var<-dfs$delta_mhmvali0010
dfs$deltamhmvali4cat<-cut(var, breaks=4)

dfs$deltahisp0010<-(dfs$t10_ldb_hisp_p_2012-dfs$t10_ldb_hisp_p_2000)
var<-dfs$delta_hisp0010
dfs$deltamhisp4cat<-cut(var, breaks=4)

dfs$deltaprof0010<-(dfs$t10_ldb_prof_p_2012-dfs$t10_ldb_prof_p_2000)
var<-dfs$deltaprof0010
dfs$deltaprof4cat<-cut(var, breaks=4)

########################################
#prep the greenspace variables (redo of data prep so that cut points are based on distribution in this data subset):
########################################

dfs$s_deltagreen0010<-scale(dfs$green_p_2011-dfs$green_p_2001)

var<-dfs$green_p_2001
p01=quantile(var, p=c(0.75), na.rm=T)
dfs$greenp2001cat<-ifelse(var<p01,0,1)
p4=quantile(var, p=c(0, 0.25, 0.50, 0.75), na.rm=T)

dfs$greenp2001_4cat<-cut(var, breaks=4)

var<-dfs$green_p_1992
p01=quantile(var, p=c(0.75), na.rm=T)
dfs$greenp1992cat<-ifelse(var<p01,0,1)
dfs$greenp1992_4cat<-cut(var, breaks=4)

var<-dfs$tree_p_2001
p01=quantile(var, p=c(0.75), na.rm=T)
dfs$treep2001cat<-ifelse(var<p01,0,1)
dfs$treep2001_4cat<-cut(var, breaks=4)

var<-dfs$tree_p_1992
p01=quantile(var, p=c(0.75), na.rm=T)
dfs$treep1992cat<-ifelse(var<p01,0,1)
dfs$treep1992_4cat<-cut(var, breaks=4)

var<-dfs$open_p_1992
p01=quantile(var, p=c(0.75), na.rm=T)
dfs$openp1992cat<-ifelse(var<p01,0,1)
dfs$openp1992_4cat<-cut(var, breaks=4)

var<-dfs$open_p_2001
p01=quantile(var, p=c(0.75), na.rm=T)
dfs$openp2001cat<-ifelse(var<p01,0,1)
dfs$openp2001_4cat<-cut(var, breaks=4)

var<-dfs$t10_ldb_pop_d_1990
p4=quantile(var, p=c(0.25, 0.50, 0.75), na.rm=T)
dfs$pop_d_90_4cat<-ifelse(dfs$t10_ldb_pop_d_1990< p4[1], 0,
                          ifelse(dfs$t10_ldb_pop_d_1990>=p4[1] & dfs$t10_ldb_pop_d_1990<p4[2], 1,
                                 ifelse(dfs$t10_ldb_pop_d_1990>=p4[2] & dfs$t10_ldb_pop_d_1990<p4[3], 2,3)))

var<-dfs$t10_ldb_pop_d_2000
p4=quantile(var, p=c(0.25, 0.50, 0.75), na.rm=T)
dfs$pop_d_00_4cat<-ifelse(dfs$t10_ldb_pop_d_2000< p4[1], 0,
                          ifelse(dfs$t10_ldb_pop_d_2000>=p4[1] & dfs$t10_ldb_pop_d_2000<p4[2], 1,
                                 ifelse(dfs$t10_ldb_pop_d_2000>=p4[2] & dfs$t10_ldb_pop_d_2000<p4[3], 2,3)))

#create a four-level categorical variable for the greenspace variables based on their internal
#distribution in the data:

#0-50th percentile, >=50th percentile-75%, >=75th percentile
#summary(dfs$green_p_1992)

#Min.     1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.07977 0.25761 0.36509 0.62371 1.00000
var<-dfs$green_p_1992
p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
dfs$greenp1992_3cat<-ifelse(var< p3[1], 0,
                          ifelse(var>=p3[1] & var < p3[2], 1,2))

#summary(dfs$green_p_2001)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.05849 0.21482 0.34879 0.99971 

dfs$greenp2001_4cat<-ifelse(dfs$green_p_1992<0.07977, 0,
                            ifelse(dfs$green_p_1992>=0.07977 & dfs$green_p_1992<0.25761,1,
                                   ifelse(dfs$green_p_1992>=0.25761 & dfs$green_p_1992<0.62371,2,
                                          ifelse(dfs$green_p_1992>=0.25761 & dfs$green_p_1992<0.62371,2,3))))


library(haven)

gentrification <- read_sas("ct10gentrif_us_90_10 (1).sas7bdat")#72538


#take out the gent measures to put them in again
#dfs_d<-dfs[c(-126:-144)]
dfs_d<-dfs[c(-129:-144)]
dfs_d$indicator<-1
dfs_g<-merge(dfs_d, gentrification, by.x="FIPS", by.y="geoid10", all.x=T)#36337

# THAT WORKED!!! 

dfs_g9000<-subset(dfs_g, dfs_g$t10_ldb_gen1_9000 !=9) #27178
dfs_g0010<-subset(dfs_g, dfs_g$t10_ldb_gen1_0010 !=9) #27220

dfs_g9000$anygent9000<-ifelse(dfs_g9000$t10_ldb_gen2_9000==0,0,
                          ifelse(dfs_g9000$t10_ldb_gen2_9000==1,1,NA))

dfs_g0010$anygent0010<-ifelse(dfs_g0010$t10_ldb_gen2_0010==0,0,
                          ifelse(dfs_g0010$t10_ldb_gen2_0010==1,1,NA))

dfs_g9000$intgent9000<-ifelse(dfs_g9000$t10_ldb_gen1_9000==2,1,
                          ifelse(dfs_g9000$t10_ldb_gen1_9000==0,0,
                                 ifelse(dfs_g9000$t10_ldb_gen1_9000==1,NA,NA)))

dfs_g0010$intgent0010<-ifelse(dfs_g0010$t10_ldb_gen1_0010==2,1,
                          ifelse(dfs_g0010$t10_ldb_gen1_0010==0,0,
                                 ifelse(dfs_g0010$t10_ldb_gen1_0010==1,NA,NA)))


# DESCRIPTIVE BY MSA

df_g9000<-as.data.frame(dfs_g9000)
df_g0010<-as.data.frame(dfs_g0010)


library(dplyr)

a<-df_g9000 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(green_p_1992, 0.25),
            q2 = quantile(green_p_1992, 0.50),
            q3 = quantile(green_p_1992, 0.75),
            min=min(green_p_1992),
            max=max(green_p_1992))

b<-df_g0010 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(green_p_2001, 0.25),
            q2 = quantile(green_p_2001, 0.50),
            q3 = quantile(green_p_2001, 0.75),
            min=min(green_p_2001),
            max=max(green_p_2001))

a<-df_g9000 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(green_p_1992, 0.25),
            q2 = quantile(green_p_1992, 0.50),
            q3 = quantile(green_p_1992, 0.75),
            min=min(green_p_1992),
            max=max(green_p_1992))

c<-df_g0010 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_pop_d_1990, 0.25),
            q2 = quantile(t10_ldb_pop_d_1990, 0.50),
            q3 = quantile(t10_ldb_pop_d_1990, 0.75),
            min=min(t10_ldb_pop_d_1990),
            max=max(t10_ldb_pop_d_1990))

d<-df_g9000 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_pop_d_2000, 0.25),
            q2 = quantile(t10_ldb_pop_d_2000, 0.50),
            q3 = quantile(t10_ldb_pop_d_2000, 0.75),
            min=min(t10_ldb_pop_d_2000),
            max=max(t10_ldb_pop_d_2000))

e<-df_g0010 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_nhblk_p_1990, 0.25),
            q2 = quantile(t10_ldb_nhblk_p_1990, 0.50),
            q3 = quantile(t10_ldb_nhblk_p_1990, 0.75),
            min=min(t10_ldb_nhblk_p_1990),
            max=max(t10_ldb_nhblk_p_1990))


f<-df_g9000 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_nhblk_p_2000, 0.25),
            q2 = quantile(t10_ldb_nhblk_p_2000, 0.50),
            q3 = quantile(t10_ldb_nhblk_p_2000, 0.75),
            min=min(t10_ldb_nhblk_p_2000),
            max=max(t10_ldb_nhblk_p_2000))

g<-df_g0010 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_hisp_p_1990, 0.25),
            q2 = quantile(t10_ldb_hisp_p_1990, 0.50),
            q3 = quantile(t10_ldb_hisp_p_1990, 0.75),
            min=min(t10_ldb_hisp_p_1990),
            max=max(t10_ldb_hisp_p_1990))


h<-df_g9000 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_hisp_p_2000, 0.25),
            q2 = quantile(t10_ldb_hisp_p_2000, 0.50),
            q3 = quantile(t10_ldb_hisp_p_2000, 0.75),
            min=min(t10_ldb_hisp_p_2000),
            max=max(t10_ldb_hisp_p_2000))

i<-df_g9000 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_nhwht_p_1990, 0.25),
            q2 = quantile(t10_ldb_nhwht_p_1990, 0.50),
            q3 = quantile(t10_ldb_nhwht_p_1990, 0.75),
            min=min(t10_ldb_nhwht_p_1990),
            max=max(t10_ldb_nhwht_p_1990))


j<-df_g0010 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(q1 = quantile(t10_ldb_nhwht_p_2000, 0.25),
            q2 = quantile(t10_ldb_nhwht_p_2000, 0.50),
            q3 = quantile(t10_ldb_nhwht_p_2000, 0.75),
            min=min(t10_ldb_nhwht_p_2000),
            max=max(t10_ldb_nhwht_p_2000))

df_g9000$count<-1
df_g0010$count<-1


k<-df_g9000 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(count = sum(count))
         
l<-df_g0010 %>% 
  group_by(m10_cen_uid_u_2010) %>% 
  summarize(count = sum(count))

all_90<-cbind(a,c[2:6],e[2:6],g[2:6],i[2:6],k[2])
all_00<-cbind(b,d[2:6],f[2:6],h[2:6],j[2:6], l[2])

all_902<-round(all_90[2:27], 2)

all_902$CBSA<-all_90$m10_cen_uid_u_2010



all_002<-round(all_00[2:27], 2)
all_002$CBSA<-all_00$m10_cen_uid_u_2010

colnames(all_902)<-c(
  '% green 1992 q1','% green 1992 q2','% green 1992 q3','% green 1992 min','% green 1992 max',
  "Population density q1","Population density q2","Population density q3","Population density min","Population density max",
  "Non-Hispanic Black q1", "Non-Hispanic Black q2", "Non-Hispanic Black q3","Non-Hispanic Black min","Non-Hispanic Black max",
  "Hispanic q1", "Hispanic q2", "Hispanic q3", "Hispanic min","Hispanic max",
  "Non-Hispanic white q1","Non-Hispanic white q2","Non-Hispanic white q3",
  "Non-Hispanic white min","Non-Hispanic white max","No. census tracts", 'CBSA')

colnames(all_002)<-c(
  '% green 2001 q1','% green 2001 q2','% green 2001 q3','% green 2001 min','% green 2001 max',
  "Population density q1","Population density q2","Population density q3","Population density min","Population density max",
  "Non-Hispanic Black q1", "Non-Hispanic Black q2", "Non-Hispanic Black q3","Non-Hispanic Black min","Non-Hispanic Black max",
  "Hispanic q1", "Hispanic q2", "Hispanic q3", "Hispanic min","Hispanic max",
  "Non-Hispanic white q1","Non-Hispanic white q2","Non-Hispanic white q3", 
  "Non-Hispanic white min","Non-Hispanic white max", "No. census tracts",'CBSA')


setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis")
cbsa<-read_excel("cbsa_cityname_8_20_20.xlsx")

cbsa$CBSA<-as.factor(cbsa$CBSA)

all_90f<-merge(all_902, cbsa, by="CBSA", all.x=T)

all_00f<-merge(all_002, cbsa, by="CBSA", all.x=T)



setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(all_90f, "20200828_descriptive_bymsa_9000.csv")
write.csv(all_00f, "20200828_descriptive_bymsa_0010.csv")


####### CORRELATION PLOTS

deltavars9000<-c("green_p_1992", "t10_ldb_pop_d_1990", "deltablk9000", "deltawht9000", "deltapov9000", "delta_col_p9000", 
                 "delta_hinci9000", "delta_mrenti9000", "delta_mhmval9000",
                 "deltahisp9000", "deltaprof9000" )


deltavars0010<-c("green_p_2001", "t10_ldb_pop_d_2000", "deltablk0010", "deltawht0010", "deltapov1000", "delta_col_p0010", 
                 "delta_hinci0010", "delta_mrenti0010", "delta_mhmvali0010",
                 "deltahisp0010", "deltaprof0010" )


s_9000<-as.data.frame(dfs_g9000[deltavars9000])
s_0010<-as.data.frame(dfs_g0010[deltavars0010])

colnames(s_9000)<-c("% green 1992",  "Population Density, 1990", "Delta % NH Black", "Delta % NH white", "Delta % Poverty", "Delta % College", "Delta Household income",
                       "Delta rent", "Delta Home value", "Delta % Hispanic", "Delta % Professional jobs")

colnames(s_0010)<-c("% green 2001", "Population Density, 2000","Delta % NH Black", "Delta % NH white", "Delta % Poverty", "Delta % College", "Delta Household income",
                       "Delta rent", "Delta Home value", "Delta % Hispanic", "Delta % Professional jobs")

dfs_g9000n<-s_9000[c("% green 1992","Population Density, 1990",
                         "Delta % NH Black",
                        "Delta % NH white","Delta % Hispanic", "Delta % College",
                        "Delta % Professional jobs",
                        "Delta % Poverty",  "Delta Household income",
                        "Delta rent", "Delta Home value")]

dfs_g0010n<-s_0010[c("% green 2001","Population Density, 2000", 
                        "Delta % NH Black", 
                        "Delta % NH white","Delta % Hispanic", 
                        "Delta % College","Delta % Professional jobs",
                        "Delta % Poverty",  "Delta Household income",
                        "Delta rent", "Delta Home value")]                   

library(corrplot)


res1 <- round(cor(dfs_g9000n),2)
corrplot(res1,  
         type="upper", order="hclust") # Add coefficient of correlation)

res2 <- round(cor(dfs_g0010n),2)
corrplot(res2,    type="upper", order="hclust")

cordf90<-as.data.frame(res1)
cordf00<-as.data.frame(res2)

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(cordf90, "correlationcoef_19902000.csv")
write.csv(cordf00, "correlationcoef_20002010.csv")



##############################################################################################

#GLM models, categorical gent vars
##############################################################################################

#1992 

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data

coefall <- matrix(NA,nrow(cbsa),4, dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-ifelse(var<=p3[1], 0,
                              ifelse(var>p3[1] & var <= p3[2], 1,2))
  

  
    # RUN THE MODELS 
  
  f1 <- glm(anygent9000~as.factor(greenp1992_3cat), data=d, family =  binomial(link="log") )

  #extract coefficients
  coefall[i,1:2] <- summary(f1)$coefficients[2,1:2]
  coefall[i,3:4] <- summary(f1)$coefficients[3,1:2]
  
  
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1", "b2", "se2")
out$greenvar<-"green 1992, 3cat"
out$gentvar<-"any gent 1992"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200818_green3cat_anygentcat_9000_glm.csv")

########2001!!!!!!

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space")
cbsa<-read.csv("v2_region_names_cbsa.csv")

####################################################

#matrix to store data
coefall <- matrix(NA,nrow(cbsa),2, dimnames=list(regions, paste("b", seq(2), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p3=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat_d2<-ifelse(var>p3, 1,0)

  
  
  adj_var<-d$t10_ldb_pop_d_2000
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  
  
  # RUN THE MODELS 
  
  f1 <- glm(anygent0010~as.factor(greenp2001cat) , data=d, family =  binomial(link="log") )
  
  #extract coefficients
  coefall[i,1:2] <- summary(f1)$coefficients[2,1:2]
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1")
out$greenvar<-"green 2001, >75%"
out$gentvar<-"any gent 2000-2010"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200818_green2cat75_anygentcat_0010_glm.csv")

# adjust for population density


#1992 

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data

coefall <- matrix(NA,nrow(cbsa),4, dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-ifelse(var<=p3[1], 0,
                            ifelse(var>p3[1] & var <= p3[2], 1,2))
  
  
  
  adj_var<-d$t10_ldb_pop_d_1990
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_1990<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  
  # RUN THE MODELS 
  
  f1 <- glm(anygent9000~as.factor(greenp1992_3cat) + 
              popden4cat_1990, data=d, family =  "poisson", 
            start=c(0.1, 0.1, 0.1, 0.1,0.1, 0.1))
  
  #extract coefficients
  coefall[i,1:2] <- summary(f1)$coefficients[2,1:2]
  coefall[i,3:4] <- summary(f1)$coefficients[3,1:2]
  
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1", "b2", "se2")
out$greenvar<-"green 1992, 3cat"
out$gentvar<-"any gent 1992"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200818_green3cat_anygentcat_9000_glm_adjusted.csv")

########2001!!!!!!

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space")
cbsa<-read.csv("v2_region_names_cbsa.csv")

####################################################

#matrix to store data
coefall <- matrix(NA,nrow(cbsa),2, dimnames=list(regions, paste("b", seq(2), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p3=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat_d2<-ifelse(var>p3, 1,0)
  
  
  adj_var<-d$t10_ldb_pop_d_2000
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_2000<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
# RUN THE MODELS 
  
  #NOTE: RUN USING POISSON LINK B/C OF MODEL CONVERGENCE ISSUES
  f1 <- glm(anygent0010~as.factor(greenp2001cat) + as.factor(popden4cat_2000), data=d, family =  'poisson' )
  
  #extract coefficients
  coefall[i,1:2] <- summary(f1)$coefficients[2,1:2]
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1")
out$greenvar<-"green 2001, >75%"
out$gentvar<-"any gent 2000-2010"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200818_green2cat75_anygentcat_0010_glm_adjusted.csv")

###################
###############################################################################

#Spatial lag models for linear demographic change vars

#1992 (1990-2000) models



###############################################################################

#Spatial lag models for linear demographic change vars

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "b2", "b2_se", "linear_gent_var") ### NOTE! THESE ARE MIS_LABELED! SHOULD BE B1 B2 B1SE B2SE make sure to relabel for meta analysis and figures!!!

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_gent_green3cat92_cityspecific_spatial.csv")

############################################

# RUN MODELS ADJUSTED FOR POP DENSITY

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  
  
  adj_var<-d$t10_ldb_pop_d_1990
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_1990<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  
  
  
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(d$s_deltablk9000~d$greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat+popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b2", "b1_se", "b2_se", "linear_gent_var")

allcc$adjust<-"adjusted for pop density"
setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_gent_green3cat92_citysp_popdensadj_spatial.csv")


#### EFFECT MODIFICATION BY race/ethnic makeup##########################

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp1992cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_nhblk_p_1990
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_linear_green92_effbyblkcat.csv")

##################################

########### MODIFICATION BY HISPANIC


# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp1992cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_hisp_p_1990
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_linear_green92_effbyhispcat.csv")

##################################

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#### EFFECT MODIFICATION BY race/ethnic makeup##########################

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp1992cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_nhblk_p_1990
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  
  adj_var<-d$t10_ldb_pop_d_1990
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_1990<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_wht + blkcat_wht:greenp1992cat + popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200928_linear_green92_effbyblkcat_adjustedorpopdens.csv")

##################################

########### MODIFICATION BY HISPANIC

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp1992cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_hisp_p_1990
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  adj_var<-d$t10_ldb_pop_d_1990
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_1990<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_wht + blkcat_wht:greenp1992cat + popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat+ popden4cat_1990, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200928_linear_green92_effbyhispcat_adjpopdens.csv")

##################################

### EMM by population density

##################################################################################################################
# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp1992cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_pop_d_1990
  p75r=quantile(racevar, p=c(0.75), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p75r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p75r[1], 1,0)
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_wht + blkcat_wht:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk9000~greenp1992cat + blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992cat+ blkcat_blk + blkcat_blk:greenp1992cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_linear_green92_effbypopdens.csv")

##################################


###############################################################
# sensitivity analyses ##
###############################################################


# Rook matrix instead of queens

# dont exclude 'non gentrifiable census tracts #

###############################################################
# sensitivity analyses ##
###############################################################


regions <- as.character(unique(dfs_g$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g[dfs_g$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(s_deltablk9000~d$greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "b2", "b2_se", "linear_gent_var") ### NOTE! THESE ARE MIS_LABELED! SHOULD BE B1 B2 B1SE B2SE make sure to relabel for meta analysis and figures!!!

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200909_gent_green3cat92_sensitivity_alltracts.csv")

# Rook matrix instead of queens


regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(s_deltablk9000~d$greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "b2", "b2_se", "linear_gent_var") ### NOTE! THESE ARE MIS_LABELED! SHOULD BE B1 B2 B1SE B2SE make sure to relabel for meta analysis and figures!!!

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200909_gent_green3cat92_sensitivity_rook.csv")

#########DEFINE AS ELIGIBLE TO GENTRIFY CENSUS TRACTS 
#WITH < 50% MEDIAN INCOME RELATIVE TO THE MSA to which they belong

dfs_g9000<-subset(dfs_g, dfs_g$t10_ldb_gen3_9000 !=9) #18108
#dfs_g0010<-subset(dfs_g, dfs_g$t10_ldb_gen3_0010 !=9) #18064


regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(s_deltablk9000~d$greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "b2", "b2_se", "linear_gent_var") ### NOTE! THESE ARE MIS_LABELED! SHOULD BE B1 B2 B1SE B2SE make sure to relabel for meta analysis and figures!!!

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200909_gent_green3cat92_sensitivity_50pctcut.csv")

############################################################################


data<-read.csv("20200819_gent_green3cat92_cityspecific_spatial.csv")

#revise the column names because the SE and Betas are mislabled

colnames(data)<-c("cbsa", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

write.csv(delta92, "20201213_meta_delta92_unadjusted.csv")

#################################################

data<-read.csv('20200819_gent_green3cat92_citysp_popdensadj_spatial.csv')

colnames(data)<-c("cbsa", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

write.csv(delta92, "20201213_meta_delta92_adjusted.csv")

##########
data<-read.csv('20200819_linear_green92_effbyblkcat.csv')

colnames(data)[1]<-c("cbsa")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)
write.csv(delta92, "20201214__meta_delta92_emm_blk.csv")

##############

data<-read.csv('20200819_linear_green92_effbyhispcat.csv')

colnames(data)[1]<-c("cbsa")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

colnames(delta92)<-c("low_hisp", "low_hisp_se", "high_hisp", "high_hisp_se")

write.csv(delta92, "20201214_meta_delta92_emm_hisp.csv")

########

data<-read.csv('20200928_linear_green92_effbyblkcat_adjustedorpopdens.csv')

colnames(data)[1]<-c("cbsa")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

write.csv(delta92, "20201214__meta_delta92_emm_blk_popadj.csv")

########

data<-read.csv('20200928_linear_green92_effbyhispcat_adjpopdens.csv')

colnames(data)[1]<-c("cbsa")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b_white, sei=b_se_white, data=one, method="REML") 
a_2<-rma(b_blk, sei=b_se_blk, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_2)$beta, 
                           summary(a_2)$se))

colnames(all_1)<-c( "white", "white_se",
                    "black", "black_se")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

colnames(delta92)<-c("low_hisp", "low_hisp_se", "high_hisp", "high_hisp_se")

write.csv(delta92, "20201214__meta_delta92_emm_hisp_popadj.csv")


########

data<-read.csv('20200909_gent_green3cat92_sensitivity_alltracts.csv')

colnames(data)<-c("cbsa", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

write.csv(delta92, "20201213_meta_delta92_sensitivity_alltracts.csv")


########

#
data<-read.csv('20200909_gent_green3cat92_sensitivity_rook.csv')

colnames(data)<-c("cbsa", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

write.csv(delta92, "20201213_meta_delta92_sensitivity_rooks.csv")

########




#
data<-read.csv('20200909_gent_green3cat92_sensitivity_50pctcut.csv')

colnames(data)<-c("cbsa", "b1", "b2", "b1_se", "b2_se", "linear_gent_var")

one<-data[which(data$linear_gent_var=="delta blk 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, deltablk 1992, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hinci 1992, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta poverty 1992, no adjustment"

deltapov<-all_1


one<-data[which(data$linear_gent_var=="delta college ed 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta college 1992, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta hispanic 1992, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta professional 1992, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta white 1992, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta median home value 1992, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 90-00"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
a_2<-rma(b2, sei=b2_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2, 
                           summary(a_2)$beta, summary(a_2)$se, summary(a_2)$I2))

colnames(all_1)<-c( "greenp01_q2", "greenp01_q2_se","greenp01_i2",
                    "greenp01_q3", "greenp01_q3_se", "greenp01_i2")

all_1$descriptor<-"% green 1992, delta household rent 1992, no adjustment"

deltarent<-all_1

delta92<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)

write.csv(delta92, "20201213_meta_delta92_sens_50pctcut.csv")

########

library(ggplot2)
library(tidyr)

gent92<-read.csv("20201213_meta_delta92_adjusted.csv")
gent92_adj<-read.csv("20201213_meta_delta92_adjusted.csv")


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

tiff("20201214_green1992_deltavars_unadjusted_gentrifiable.tiff", units="in", 
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

#### 2010 models #####

############################################

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),2, 
                                           dimnames=list(regions, paste("b (se)", seq(2), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p3=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-as.factor(ifelse(var>p3, 1,0))
  
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(d$s_deltablk0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1] <- summary(f1)$coefficients[2]
  c1[i,2] <- summary(f1)$rest.se[2]
  c2[i,1] <- summary(f2)$coefficients[2]
  c2[i,2] <- summary(f2)$rest.se[2]
  c3[i,1] <- summary(f3)$coefficients[2]
  c3[i,2] <- summary(f3)$rest.se[2]
  c4[i,1] <- summary(f4)$coefficients[2]
  c4[i,2] <- summary(f4)$rest.se[2]
  c5[i,1] <- summary(f5)$coefficients[2]
  c5[i,2] <- summary(f5)$rest.se[2]
  c6[i,1] <- summary(f6)$coefficients[2]
  c6[i,2] <- summary(f6)$rest.se[2]
  c7[i,1] <- summary(f7)$coefficients[2]
  c7[i,2] <- summary(f7)$rest.se[2]
  c8[i,1] <- summary(f8)$coefficients[2]
  c8[i,2] <- summary(f8)$rest.se[2]
  c9[i,1] <- summary(f9)$coefficients[2]
  c9[i,2] <- summary(f9)$rest.se[2]
  }

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"


allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_gent_green2cat01_cityspecific_spatial.csv")

# RUN MODELS ADJUSTED FOR POP DENSITY

############################################

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),2, 
                                           dimnames=list(regions, paste("b (se)", seq(2), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p3=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-as.factor(ifelse(var>p3, 1,0))
  
  adj_var<-d$t10_ldb_pop_d_2000
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_2000<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(d$s_deltablk0010~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+popden4cat_2000+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1] <- summary(f1)$coefficients[2]
  c1[i,2] <- summary(f1)$rest.se[2]
  c2[i,1] <- summary(f2)$coefficients[2]
  c2[i,2] <- summary(f2)$rest.se[2]
  c3[i,1] <- summary(f3)$coefficients[2]
  c3[i,2] <- summary(f3)$rest.se[2]
  c4[i,1] <- summary(f4)$coefficients[2]
  c4[i,2] <- summary(f4)$rest.se[2]
  c5[i,1] <- summary(f5)$coefficients[2]
  c5[i,2] <- summary(f5)$rest.se[2]
  c6[i,1] <- summary(f6)$coefficients[2]
  c6[i,2] <- summary(f6)$rest.se[2]
  c7[i,1] <- summary(f7)$coefficients[2]
  c7[i,2] <- summary(f7)$rest.se[2]
  c8[i,1] <- summary(f8)$coefficients[2]
  c8[i,2] <- summary(f8)$rest.se[2]
  c9[i,1] <- summary(f9)$coefficients[2]
  c9[i,2] <- summary(f9)$rest.se[2]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "linear_gent_var")

allcc$adjust<-"adjusted for pop density"
setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_gent_green2cat01_citysp_popdensadj_spatial.csv")

#### EFFECT MODIFICATION BY race/ethnic makeup##########################

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_nhblk_p_2000
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_linear_green01_effbyblkcat.csv")

########### MODIFICATION BY HISPANIC

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_hisp_p_2000
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_linear_green01_effbyhispcat.csv")

##########################################################################################################

### EFFECT MODIFICATION AGAIN WITH ADJUSTMENT FOR POPULATION DENSITY ######################################

##################################

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_nhblk_p_2000
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  
  adj_var<-d$t10_ldb_pop_d_2000
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_2000<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_wht + blkcat_wht:greenp2001cat + popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat + popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200928_linear_green01_effbyblkcat_adjpopdens.csv")

########### MODIFICATION BY HISPANIC

# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_hisp_p_2000
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  
  
  adj_var<-d$t10_ldb_pop_d_2000
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_2000<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat+ popden4cat_2000, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200928_linear_green01_effbyhispcat_adjpopdens.csv")

### EMM by population density


# ARRANGE THE DATA AS A LIST OF DATA SETS

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),5, dimnames=list(regions, paste("b", seq(5), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p75=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-ifelse(var<=p75[1], 0,1)
  
  racevar<-d$t10_ldb_pop_d_2000
  
  p50r=quantile(racevar, p=c(0.50), na.rm=T)
  d$blkcat_wht<-ifelse(racevar<=p50r[1], 0,1)
  d$blkcat_blk<-ifelse(racevar<=p50r[1], 1,0)
  
  # RUN THE MODELS 
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1_race0 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race0 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race0 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race0 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race0 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race0 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race0 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race0 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race0 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_wht + blkcat_wht:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  
  f1_race1 <- spatialreg::lagsarlm(s_deltablk0010~greenp2001cat + blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2_race1 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3_race1 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4_race1 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5_race1 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6_race1 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7_race1 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8_race1 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9_race1 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat+ blkcat_blk + blkcat_blk:greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  
  
  #extract coefficients
  c1[i,1] <- summary(f1_race0)$coefficients[2]
  c1[i,2] <- summary(f1_race0)$rest.se[2]
  c2[i,1] <- summary(f2_race0)$coefficients[2]
  c2[i,2] <- summary(f2_race0)$rest.se[2]
  c3[i,1] <- summary(f3_race0)$coefficients[2]
  c3[i,2] <- summary(f3_race0)$rest.se[2]
  c4[i,1] <- summary(f4_race0)$coefficients[2]
  c4[i,2] <- summary(f4_race0)$rest.se[2]
  c5[i,1] <- summary(f5_race0)$coefficients[2]
  c5[i,2] <- summary(f5_race0)$rest.se[2]
  c6[i,1] <- summary(f6_race0)$coefficients[2]
  c6[i,2] <- summary(f6_race0)$rest.se[2]
  c7[i,1] <- summary(f7_race0)$coefficients[2]
  c7[i,2] <- summary(f7_race0)$rest.se[2]
  c8[i,1] <- summary(f8_race0)$coefficients[2]
  c8[i,2] <- summary(f8_race0)$rest.se[2]
  c9[i,1] <- summary(f9_race0)$coefficients[2]
  c9[i,2] <- summary(f9_race0)$rest.se[2]
  
  c1[i,3] <- summary(f1_race1)$coefficients[2]
  c1[i,4] <- summary(f1_race1)$rest.se[2]
  c2[i,3] <- summary(f2_race1)$coefficients[2]
  c2[i,4] <- summary(f2_race1)$rest.se[2]
  c3[i,3] <- summary(f3_race1)$coefficients[2]
  c3[i,4] <- summary(f3_race1)$rest.se[2]
  c4[i,3] <- summary(f4_race1)$coefficients[2]
  c4[i,4] <- summary(f4_race1)$rest.se[2]
  c5[i,3] <- summary(f5_race1)$coefficients[2]
  c5[i,4] <- summary(f5_race1)$rest.se[2]
  c6[i,3] <- summary(f6_race1)$coefficients[2]
  c6[i,4] <- summary(f6_race1)$rest.se[2]
  c7[i,3] <- summary(f7_race1)$coefficients[2]
  c7[i,4] <- summary(f7_race1)$rest.se[2]
  c8[i,3] <- summary(f8_race1)$coefficients[2]
  c8[i,4] <- summary(f8_race1)$rest.se[2]
  c9[i,3] <- summary(f9_race1)$coefficients[2]
  c9[i,4] <- summary(f9_race1)$rest.se[2]
  
  # P value for the interaction term
  c1[i,5] <- summary(f1_race1)$Coef[4,4]
  c2[i,5] <- summary(f2_race1)$Coef[4,4]
  c3[i,5] <- summary(f3_race1)$Coef[4,4]
  c4[i,5] <- summary(f4_race1)$Coef[4,4]
  c5[i,5] <- summary(f5_race1)$Coef[4,4]
  c6[i,5] <- summary(f6_race1)$Coef[4,4]
  c7[i,5] <- summary(f7_race1)$Coef[4,4]
  c8[i,5] <- summary(f8_race1)$Coef[4,4]
  c9[i,5] <- summary(f9_race1)$Coef[4,4]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b_white", "b_se_white", "b_blk", "b_se_blk", "p for interaction", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_linear_green01_effbypopdens.csv")



#####################################################################################

#Rerun the GLM models using poisson distribution because model convergence issues

# when I adjust for population density

#####################################################################################
 
# first unadjusted # 

#use df_g9000 and df_g0010 because these are nonspatial models

library(geepack)

regions <- as.character(unique(df_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) df_g9000[df_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data

coefall <- matrix(NA,nrow(cbsa),4, dimnames=list(regions, paste("b", seq(4), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-ifelse(var<=p3[1], 0,
                            ifelse(var>p3[1] & var <= p3[2], 1,2))
  
  #create a sequence number variable b/c running GEE models
  d$row_num <- seq.int(nrow(d)) 
  
  # RUN THE MODELS 
  
 f1 <- geeglm(anygent9000~as.factor(greenp1992_3cat), data=d, family =  poisson(link = "log"),
          id = row_num,  corstr = "exchangeable")
  
#f1 <- glm(anygent9000~as.factor(greenp1992_3cat), data=d, family =  poisson(link = "log"))
  
  #extract coefficients
  coefall[i,1] <- summary(f1)$coefficients[2,1]
  coefall[i,2] <- summary(f1)$coefficients[2,2]
  
  coefall[i,3] <- summary(f1)$coefficients[3,1]
  coefall[i,4] <- summary(f1)$coefficients[3,2]
  
  
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1", "b2", "se2")
out$greenvar<-"green 1992, 3cat"
out$gentvar<-"any gent 1992"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200908_green3cat_anygentcat_9000_poissonrobust.csv")

########2001!!!!!!


regions <- as.character(unique(df_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) df_g0010[df_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data

coefall <- matrix(NA,nrow(cbsa),2, dimnames=list(regions, paste("b", seq(2), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p2=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001_2cat<-ifelse(var<=p2[1], 0,1)

  #create a sequence number variable b/c running GEE models
  d$row_num <- seq.int(nrow(d)) 
  
  # RUN THE MODELS 
  
  f1 <- geeglm(anygent0010~as.factor(greenp2001_2cat), data=d, family =  poisson(link = "log"),
               id = row_num,  corstr = "exchangeable")
  
  #f1 <- glm(anygent9000~as.factor(greenp1992_3cat), data=d, family =  poisson(link = "log"))
  
  #extract coefficients
  coefall[i,1] <- summary(f1)$coefficients[2,1]
  coefall[i,2] <- summary(f1)$coefficients[2,2]
  
  
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1")
out$greenvar<-"green 2001, 2cat"
out$gentvar<-"any gent 2001"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200908_green2cat_anygentcat_0010_poissonrobust.csv")

##########


#adjusted for population density

regions <- as.character(unique(df_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) df_g9000[df_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

coefall <- matrix(NA,nrow(cbsa),4, dimnames=list(regions, paste("b", seq(4), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-ifelse(var<=p3[1], 0,
                            ifelse(var>p3[1] & var <= p3[2], 1,2))
  
  
  
  adj_var<-d$t10_ldb_pop_d_1990
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_1990<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  
  
  #create a sequence number variable b/c running GEE models
  d$row_num <- seq.int(nrow(d)) 
  
  # RUN THE MODELS 
  
  f1 <- geeglm(anygent9000~as.factor(greenp1992_3cat) + as.factor(popden4cat_1990), data=d, family =  poisson(link = "log"),
               id = row_num,  corstr = "exchangeable")
  
  #f1 <- glm(anygent9000~as.factor(greenp1992_3cat), data=d, family =  poisson(link = "log"))
  
  #extract coefficients
  coefall[i,1] <- summary(f1)$coefficients[2,1]
  coefall[i,2] <- summary(f1)$coefficients[2,2]
  
  coefall[i,3] <- summary(f1)$coefficients[3,1]
  coefall[i,4] <- summary(f1)$coefficients[3,2]
  
  
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1", "b2", "se2")
out$greenvar<-"green 1992, 3cat"
out$gentvar<-"any gent 1992 popdens adjusted"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200908_green3cat_anygent_9000_adjusted_poissonrobust.csv")

########2001!!!!!!


regions <- as.character(unique(df_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) df_g0010[df_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data

coefall <- matrix(NA,nrow(cbsa),2, dimnames=list(regions, paste("b", seq(2), sep="" )))

for(i in seq(nrow(cbsa))) {
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p2=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001_2cat<-ifelse(var<=p2[1], 0,1)
  
  
  adj_var<-d$t10_ldb_pop_d_2000
  p4=quantile(adj_var, p=c(0.25, 0.50, 0.75), na.rm=T)
  d$popden4cat_2000<-as.factor(ifelse(adj_var<p4[1], 0,
                                      ifelse(adj_var>p4[1] & adj_var <= p4[2], 1,
                                             ifelse(adj_var>p4[2] & adj_var <= p4[3], 2,3))))
  
  #create a sequence number variable b/c running GEE models
  d$row_num <- seq.int(nrow(d)) 
  
  # RUN THE MODELS 
  
  f1 <- geeglm(anygent0010~as.factor(greenp2001_2cat) + popden4cat_2000, data=d, family =  poisson(link = "log"),
               id = row_num,  corstr = "exchangeable")
  
  #f1 <- glm(anygent9000~as.factor(greenp1992_3cat), data=d, family =  poisson(link = "log"))
  
  #extract coefficients
  coefall[i,1] <- summary(f1)$coefficients[2,1]
  coefall[i,2] <- summary(f1)$coefficients[2,2]
  
  
}

out<- data.frame(cbsa = row.names(coefall), coefall)
colnames(out)<-c("cbsa", "b1", "se1")
out$greenvar<-"green 2001, 2cat"
out$gentvar<-"any gent 2001 pop dens adj"

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(out, "20200908_green2cat_anygent_0010_adjusted_poissonrobust.csv")
###################### SENSITIVITY ANALYSES########################

###############################################################
# sensitivity analyses ##
###############################################################


# Rook matrix instead of queens


############################################

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),2, 
                                           dimnames=list(regions, paste("b (se)", seq(2), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_2001
  
  p3=quantile(var, p=c(0.75), na.rm=T)
  d$greenp2001cat<-as.factor(ifelse(var>p3, 1,0))
  
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d, queen=FALSE) #queen=false for sen analysis 
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  f1 <- spatialreg::lagsarlm(d$s_deltablk0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov1000~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof0010~greenp2001cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1] <- summary(f1)$coefficients[2]
  c1[i,2] <- summary(f1)$rest.se[2]
  c2[i,1] <- summary(f2)$coefficients[2]
  c2[i,2] <- summary(f2)$rest.se[2]
  c3[i,1] <- summary(f3)$coefficients[2]
  c3[i,2] <- summary(f3)$rest.se[2]
  c4[i,1] <- summary(f4)$coefficients[2]
  c4[i,2] <- summary(f4)$rest.se[2]
  c5[i,1] <- summary(f5)$coefficients[2]
  c5[i,2] <- summary(f5)$rest.se[2]
  c6[i,1] <- summary(f6)$coefficients[2]
  c6[i,2] <- summary(f6)$rest.se[2]
  c7[i,1] <- summary(f7)$coefficients[2]
  c7[i,2] <- summary(f7)$rest.se[2]
  c8[i,1] <- summary(f8)$coefficients[2]
  c8[i,2] <- summary(f8)$rest.se[2]
  c9[i,1] <- summary(f9)$coefficients[2]
  c9[i,2] <- summary(f9)$rest.se[2]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 00-10"
cc2$gentvar<-"delta whit 00-10"
cc3$gentvar<-"delta pov 00-10"
cc4$gentvar<-"delta median home value 00-10"
cc5$gentvar<-"delta household income 00-10"
cc6$gentvar<-"delta median household rent 00-10"
cc7$gentvar<-"delta college ed 00-10"
cc8$gentvar<-"delta hispanic 00-10"
cc9$gentvar<-"delta professional 00-10"


allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "linear_gent_var")

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200819_gent_green2cat01_citysp_sensitivity_rooks.csv")



# dont exclude 'non gentrifiable census tracts #

###############################################################
# sensitivity analyses ##
###############################################################


regions <- as.character(unique(dfs_g$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g[dfs_g$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(s_deltablk9000~d$greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov1000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "b2", "b2_se", "linear_gent_var") ### NOTE! THESE ARE MIS_LABELED! SHOULD BE B1 B2 B1SE B2SE make sure to relabel for meta analysis and figures!!!

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200909_gent_green3cat92_sensitivity_alltracts.csv")

# Rook matrix instead of queens



regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(s_deltablk9000~d$greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov1000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "b2", "b2_se", "linear_gent_var") ### NOTE! THESE ARE MIS_LABELED! SHOULD BE B1 B2 B1SE B2SE make sure to relabel for meta analysis and figures!!!

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200909_gent_green3cat92_sensitivity_rook.csv")

#########DEFINE AS ELIGIBLE TO GENTRIFY CENSUS TRACTS 
#WITH < 50% MEDIAN INCOME RELATIVE TO THE MSA to which they belong

dfs_g9000<-subset(dfs_g, dfs_g$t10_ldb_gen3_9000 !=9) #18108
#dfs_g0010<-subset(dfs_g, dfs_g$t10_ldb_gen3_0010 !=9) #18064


regions <- as.character(unique(dfs_g9000$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g9000[dfs_g9000$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),4, 
                                           dimnames=list(regions, paste("b (se)", seq(4), sep="" )))

for(i in seq(nrow(cbsa ))) {
  # PRINT
  cat(i,"")
  # EXTRACT THE DATA
  d <- dlist[[i]]
  
  var<-d$green_p_1992
  
  p3=quantile(var, p=c(0.50, 0.75), na.rm=T)
  d$greenp1992_3cat<-as.factor(ifelse(var<=p3[1], 0,
                                      ifelse(var>p3[1] & var <=p3[2], 1,2)))
  # RUN THE MODELS 
  
  #Turns neighborhood weight matrix into a list of all connected neighbors
  d_nbq<-poly2nb(d)  
  
  d_nbq_w<-nb2listw(d_nbq, zero.policy=T)
  
  
  f1 <- spatialreg::lagsarlm(s_deltablk9000~d$greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f2 <- spatialreg::lagsarlm(s_deltawht9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f3 <- spatialreg::lagsarlm(s_deltapov1000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f4 <- spatialreg::lagsarlm(s_delta_mhmvali9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f5 <- spatialreg::lagsarlm(s_delta_hinci9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f6 <- spatialreg::lagsarlm(s_delta_mrenti9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f7 <- spatialreg::lagsarlm(s_delta_col_p9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f8 <- spatialreg::lagsarlm(s_deltahisp9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  f9 <- spatialreg::lagsarlm(s_deltaprof9000~greenp1992_3cat, data=d, d_nbq_w, tol.solve = 1.0e-15, zero.policy=T)
  #extract coefficients
  c1[i,1:2] <- summary(f1)$coefficients[2:3]
  c1[i,3:4] <- summary(f1)$rest.se[2:3]
  c2[i,1:2] <- summary(f2)$coefficients[2:3]
  c2[i,3:4] <- summary(f2)$rest.se[2:3]
  c3[i,1:2] <- summary(f3)$coefficients[2:3]
  c3[i,3:4] <- summary(f3)$rest.se[2:3]
  c4[i,1:2] <- summary(f4)$coefficients[2:3]
  c4[i,3:4] <- summary(f4)$rest.se[2:3]
  c5[i,1:2] <- summary(f5)$coefficients[2:3]
  c5[i,3:4] <- summary(f5)$rest.se[2:3]
  c6[i,1:2] <- summary(f6)$coefficients[2:3]
  c6[i,3:4] <- summary(f6)$rest.se[2:3]
  c7[i,1:2] <- summary(f7)$coefficients[2:3]
  c7[i,3:4] <- summary(f7)$rest.se[2:3]
  c8[i,1:2] <- summary(f8)$coefficients[2:3]
  c8[i,3:4] <- summary(f8)$rest.se[2:3]
  c9[i,1:2] <- summary(f9)$coefficients[2:3]
  c9[i,3:4] <- summary(f9)$rest.se[2:3]
}

cc1<-as.data.frame(c1)
cc2<-as.data.frame(c2)
cc3<-as.data.frame(c3)
cc4<-as.data.frame(c4)
cc5<-as.data.frame(c5)
cc6<-as.data.frame(c6)
cc7<-as.data.frame(c7)
cc8<-as.data.frame(c8)
cc9<-as.data.frame(c9)

cc1$gentvar<-"delta blk 90-00"
cc2$gentvar<-"delta whit 90-00"
cc3$gentvar<-"delta pov 90-00"
cc4$gentvar<-"delta median home value 90-00"
cc5$gentvar<-"delta household income 90-00"
cc6$gentvar<-"delta median household rent 90-00"
cc7$gentvar<-"delta college ed 90-00"
cc8$gentvar<-"delta hispanic 90-00"
cc9$gentvar<-"delta professional 90-00"

allcc<-rbind(cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9)

colnames(allcc)<-c("b1", "b1_se", "b2", "b2_se", "linear_gent_var") ### NOTE! THESE ARE MIS_LABELED! SHOULD BE B1 B2 B1SE B2SE make sure to relabel for meta analysis and figures!!!

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")
write.csv(allcc, "20200909_gent_green3cat92_sensitivity_50pctcut.csv")
