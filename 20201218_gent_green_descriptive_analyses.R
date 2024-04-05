library(tableone)
library(metafor)
library(rgdal)
library(spdep)

#data frames are prepared in R code 20201218_gent_green_glmANDspatial

dfs_g90<-dfs_g[which(dfs_g$t10_ldb_gen1_9000!=9),] #27128
dfs_g00<-dfs_g[which(dfs_g$t10_ldb_gen1_0010!=9),] #27220



vars90<-c("t10_ldb_pop_d_1990","t10_ldb_hinci_m_1990.x", "t10_ldb_npov_p_1990",
          "t10_ldb_mhmvali_m_1990", "t10_ldb_mrenti_m_1990", 
          "t10_ldb_prof_p_1990","t10_ldb_col_p_1990","t10_ldb_nhwht_p_1990",
          "t10_ldb_nhblk_p_1990",
          "t10_ldb_hisp_p_1990")

vars00<-c("t10_ldb_pop_d_2000","t10_ldb_hinci_m_2000.x", "t10_ldb_npov_p_2000",
          "t10_ldb_mhmvali_m_2000", "t10_ldb_mrenti_m_2000", 
          "t10_ldb_prof_p_2000","t10_ldb_col_p_2000",
          "t10_ldb_nhwht_p_2000",
          "t10_ldb_nhblk_p_2000",
          "t10_ldb_hisp_p_2000")


d90<-as.data.frame(dfs_g90[vars90])
d00<-as.data.frame(dfs_g00[vars00])

t90<-CreateTableOne(vars=vars90, data=d90)

t90_t<-as.data.frame(print(t90, nonnormal = vars90))

t90df<-as.data.frame(print(t90))

t90_all<-cbind(t90df, t90_t)

####

t90_all$name<-c("N", "Population density", "Median household income", "% living in poverty",
                "Median home value",  "Median household rent",
                "% working professional jobs", "% with Bachelor's degree",
                "% Non Hispanic White", 
                "% Non Hispanic black", 
                "% Hispanic")

colnames(t90_all)<-c("Mean (SD) 1990", "Median (IQR) 1990", "Name")

t00<-CreateTableOne(vars=vars00, data=d00)

t00_t<-as.data.frame(print(t00, nonnormal = vars00))

t00df<-as.data.frame(print(t00))

t00_all<-cbind(t00df, t00_t)

t00_all$name<-c("N", "Population density", "Median household income", "% living in poverty",
                "Median home value",  "Median household rent",
                "% working professional jobs", "% with Bachelor's degree",
                "% Non Hispanic White", 
                "% Non Hispanic black", 
                "% Hispanic")

colnames(t00_all)<-c("Mean (SD) 2000", "Median (IQR) 2000", "Name")


####################################################################

all<-cbind(t90_all, t00_all)

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(all, "20201208_tableonedesc_eligibletogentrify.csv")




# descriptive statistics on the change variables
dfs_g$deltapov9000<-(dfs_g$t10_ldb_npov_p_2000-dfs_g$t10_ldb_npov_p_1990)
dfs_g$deltapov1000<-(dfs_g$t10_ldb_npov_p_2012-dfs_g$t10_ldb_npov_p_2000)
dfs_g$delta_col_p9000<-(dfs_g$t10_ldb_col_p_2000-dfs_g$t10_ldb_col_p_1990)
dfs_g$delta_col_p0010<-(dfs_g$t10_ldb_col_p_2012-dfs_g$t10_ldb_col_p_2000)

dfs_g$delta_hinci9000<-(dfs_g$t10_ldb_hinci_m_2000.x-dfs_g$t10_ldb_hinci_m_1990.x)
dfs_g$delta_hinci0010<-(dfs_g$t10_ldb_hinci_m_2012.x-dfs_g$t10_ldb_hinci_m_2000.x)
dfs_g$delta_mrenti9000<-(dfs_g$t10_ldb_mrenti_m_2000-dfs_g$t10_ldb_mrenti_m_1990)
dfs_g$delta_mrenti0010<-(dfs_g$t10_ldb_mrenti_m_2012-dfs_g$t10_ldb_mrenti_m_2000)

#inflation adjusted!!
dfs_g$delta_mhmval9000<-(dfs_g$t10_ldb_mhmvali_m_2000-dfs_g$t10_ldb_mhmvali_m_1990)
dfs_g$delta_mhmval0010<-(dfs_g$t10_ldb_mhmvali_m_2012-dfs_g$t10_ldb_mhmvali_m_2000)

dfs_g$deltahisp9000<-(dfs_g$t10_ldb_hisp_p_2000-dfs_g$t10_ldb_hisp_p_1990)
dfs_g$deltahisp0010<-(dfs_g$t10_ldb_hisp_p_2012-dfs_g$t10_ldb_hisp_p_2000)

dfs_g$deltahisp9000<-(dfs_g$t10_ldb_hisp_p_2000-dfs_g$t10_ldb_hisp_p_1990)
dfs_g$deltahisp0010<-(dfs_g$t10_ldb_hisp_p_2012-dfs_g$t10_ldb_hisp_p_2000)

dfs_g$deltaprof9000<-(dfs_g$t10_ldb_prof_p_2000-dfs_g$t10_ldb_prof_p_1990)
dfs_g$deltaprof0010<-(dfs_g$t10_ldb_prof_p_2012-dfs_g$t10_ldb_prof_p_2000)

dfs_g9000<-subset(dfs_g, dfs_g$t10_ldb_gen1_9000 !=9) #27178
dfs_g0010<-subset(dfs_g, dfs_g$t10_ldb_gen1_0010 !=9) #27220


vars90<-c('deltablk9000',  'deltawht9000',
          'deltapov9000', 'delta_col_p9000' ,
          'delta_hinci9000', 'delta_mrenti9000',
          'deltahisp9000','deltaprof9000', 'delta_mhmval9000')

vars10<-c('deltablk0010',  'deltawht0010',
          'deltapov1000', 'delta_col_p0010', 
          'delta_hinci0010', 'delta_mrenti0010', 
          'delta_mhmval0010',  'deltahisp0010', 
          'deltaprof0010')

dfs_g9000<-subset(dfs_g, dfs_g$t10_ldb_gen1_9000 !=9) #27178
dfs_g0010<-subset(dfs_g, dfs_g$t10_ldb_gen1_0010 !=9) #27220

d90<-as.data.frame(dfs_g90[vars90])
d00<-as.data.frame(dfs_g00[vars10])

t90<-CreateTableOne(vars=vars90, data=d90)

t90_t<-as.data.frame(print(t90, nonnormal = vars90))

t90df<-as.data.frame(print(t90))

t90_all<-cbind(t90df, t90_t)

####

t90_all$name<-c("N", "Delta black", "Delta white", "delta poverty", "Delta % College",
                "Delta Household income", "Delta household rent", "Delta % Hispanic",
                "Delta % professional", "Median home value")

colnames(t90_all)<-c("Mean (SD) 1990", "Median (IQR) 1990", "Name")

t00<-CreateTableOne(vars=vars10, data=d00)

t00_t<-as.data.frame(print(t00, nonnormal = vars10))

t00df<-as.data.frame(print(t00))

t00_all<-cbind(t00df, t00_t)

t00_all$name<-c("N", "Delta black", "Delta white", "delta poverty", "Delta % College",
                "Delta Household income", "Delta household rent", "Delta % Hispanic",
                "Delta % professional", "Median home value")

colnames(t00_all)<-c("Mean (SD) 2000", "Median (IQR) 2000", "Name")


all<-cbind(t90_all, t00_all)

setwd("C:/Users/lhs36/Dropbox/Leah Work/Gentrification and green space/RECVD analysis/Results")

write.csv(all, "20201208_tabletwo_deltavars_desc.csv")

