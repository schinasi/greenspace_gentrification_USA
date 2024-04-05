###################### SENSITIVITY ANALYSES########################


###############################################################
# sensitivity analyses for the 2000-2010 period  ##
###############################################################

# Rook matrix instead of queens

############################################

############################################

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),2, dimnames=list(regions, paste("b (se)", seq(2), sep="" )))

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
write.csv(allcc, "20201208_2000_2010_sensitivity_rook.csv")

# Sensitivity analysis: dont exclude 'non gentrifiable census tracts #

regions <- as.character(unique(dfs_g$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g[dfs_g$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)


####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),2, dimnames=list(regions, paste("b (se)", seq(2), sep="" )))

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
  d_nbq<-poly2nb(d) #queen=false for sen analysis 
  
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
write.csv(allcc, "20201208_200010_sens_includenongentrifiable.csv")

#########DEFINE AS ELIGIBLE TO GENTRIFY CENSUS TRACTS 
#WITH < 50% MEDIAN INCOME RELATIVE TO THE MSA to which they belong

#dfs_g9000<-subset(dfs_g, dfs_g$t10_ldb_gen3_9000 !=9) #18108
dfs_g0010<-subset(dfs_g, dfs_g$t10_ldb_gen3_0010 !=9) #18064

regions <- as.character(unique(dfs_g0010$m10_cen_uid_u_2010)) #43 regions/CBSA

dlist <- lapply(regions,function(x) dfs_g0010[dfs_g0010$m10_cen_uid_u_2010==x,])

names(dlist) <- regions

cities <- data.frame(
  city = regions)

####################################################

#matrix to store data
c1<-c2<-c3<-c4<-c5<-c6<-c7<-c8<-c9<-matrix(NA,nrow(cbsa),2, dimnames=list(regions, paste("b (se)", seq(2), sep="" )))

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
write.csv(allcc, "20201208_2000_2010_sensitivity_gent50pctcp.csv")

# RUN THE META_ANALYSES FOR THE SENS ANALYSES
data<-read.csv("20201208_2000_2010_sensitivity_rook.csv")
colnames(data)[1]<-"cbsa"

one<-data[which(data$linear_gent_var=="delta blk 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, deltablk 2000-2010, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta hinci 2000-2010, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 00-10"),]

a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta poverty 2000-2010, no adjustment"

deltapov<-all_1

one<-data[which(data$linear_gent_var=="delta college ed 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta college 2000-2010, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta hispanic 2000-2010, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta professional 2000-2010, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta white 2000-2010, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta median home value 2000-2010, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta household rent 2000-2010, no adjustment"

deltarent<-all_1

delta01<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)
write.csv(delta01, "20201209_meta_delta01_rooksensitivityanalysis.csv")

#### include the non gentrifiable tracts (no exclusions)

data<-read.csv("20201208_200010_sens_includenongentrifiable.csv")
colnames(data)[1]<-"cbsa"

one<-data[which(data$linear_gent_var=="delta blk 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, deltablk 2000-2010, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta hinci 2000-2010, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 00-10"),]

a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta poverty 2000-2010, no adjustment"

deltapov<-all_1

one<-data[which(data$linear_gent_var=="delta college ed 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta college 2000-2010, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta hispanic 2000-2010, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta professional 2000-2010, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta white 2000-2010, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta median home value 2000-2010, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta household rent 2000-2010, no adjustment"

deltarent<-all_1

delta01<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)
write.csv(delta01, "20201209_meta_delta01_nongentrifiablesensitivityanalysis.csv")

#### 50 pct cut-point 


data<-read.csv("20201208_2000_2010_sensitivity_gent50pctcp.csv")
colnames(data)[1]<-"cbsa"

one<-data[which(data$linear_gent_var=="delta blk 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, deltablk 2000-2010, no adjustment"

deltablk<-all_1

one<-data[which(data$linear_gent_var=="delta household income 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta hinci 2000-2010, no adjustment"

deltahinci<-all_1

one<-data[which(data$linear_gent_var=="delta pov 00-10"),]

a_1<-rma(b1, sei=b1_se, data=one, method="REML") 
all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta poverty 2000-2010, no adjustment"

deltapov<-all_1

one<-data[which(data$linear_gent_var=="delta college ed 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta college 2000-2010, no adjustment"

deltacollege<-all_1

one<-data[which(data$linear_gent_var=="delta hispanic 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta hispanic 2000-2010, no adjustment"

deltahispanic<-all_1

one<-data[which(data$linear_gent_var=="delta professional 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta professional 2000-2010, no adjustment"

deltaprofessional<-all_1

one<-data[which(data$linear_gent_var=="delta whit 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta white 2000-2010, no adjustment"

deltawhite<-all_1

one<-data[which(data$linear_gent_var=="delta median home value 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta median home value 2000-2010, no adjustment"

deltahomevalue<-all_1

one<-data[which(data$linear_gent_var=="delta median household rent 00-10"),]
a_1<-rma(b1, sei=b1_se, data=one, method="REML") 

all_1<-as.data.frame(cbind(summary(a_1)$beta, summary(a_1)$se, summary(a_1)$I2))

colnames(all_1)<-c( "greenp01_q3", "greenp01_q3_se", "greenp_i2")

all_1$descriptor<-"% green 2001, delta household rent 2000-2010, no adjustment"

deltarent<-all_1

delta01<-rbind(deltablk, deltawhite, deltahispanic, deltacollege, deltaprofessional,
               deltapov, deltahinci, deltahomevalue, deltarent)
write.csv(delta01, "20201209_meta_delta01_n50pctcutelig_sensitivity.csv")




