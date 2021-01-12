rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# 8200 metafor combine models 

#     Mark McCann developed the script 

#############
#  Purpose  #
#############

#Metafor meta analysis of final ERGM estimates

##############
#            #
#    Notes   #
#            #
##############

#   Change compared to 7115. 

# This works off the 4 category sex var

#########################
#                       #
#  Outstanding actions  #
#                       #
#########################


#########################
#                       #
#    Load packages      #
#                       #
#########################

library("metafor")

#########################
#                       #
#     Load functions    # 
#                       #
#########################

forestplot <- function(indata = NULL,
                       coefrow = NULL ,
                       variable = "",
                       varname = "",
                       conf = 95
){
  ##Create a 12 * 3 table, 12 schools in rows, coefficient, SE, and baseline/control in columns
  coef.table <- matrix(NA, nr=10, nc = 3) 
  for (i in 1:length(indata)){
    #Fill the table with varname row 1st col coef  
    coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable==varname)]
    #Fill the table with varname row 2nd col SE  
    coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable==varname)]
  }
  
  ###Add a baseline / control dummmy var  
  coef.table[1:5,3] <- 0
  coef.table[6:10,3] <- 1
  colnames(coef.table) <- c("coef","se","control")
  
  coef.table <- coef.table[!is.na(coef.table[,1]),] 
  #Run meta analysis
  metareg <- rma(yi=coef.table[,1],
                 sei=coef.table[,2],
                 #                 slab=c("Net1", "Net2", "Net3", "Net4", "Net5", "Net6"
                 #                        , "Net7", "Net8","Net9","Net10","Net11","Net12") 
                 # slab=c("Sch 1 base", "Sch 2 base", "Sch 3 base", "Sch 4 base", "Sch 5 base", "Sch 6 base"
                 #        , "Sch 1 ctrl", "Sch 2 ctrl","Sch 3 ctrl","Sch 4 ctrl","Sch 5 ctrl","Sch 6 ctrl") 
                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                        , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                 , method = "REML", level = conf)
  
  metareg.wave <- rma(yi=coef.table[,1],
                      sei=coef.table[,2], 
                      slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                             , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                      , method = "REML" 
                      , mods = coef.table[,3])
  
  print(metareg.wave)
  
  #Plot results
  setwd("\\\\192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")

  pdf(paste0(variable," forest 12 networks with outfrnds 7200 standardised new vars.pdf"))
  
  forest(metareg, 
         main = paste0("Tie probability by difference in ",variable),
         xlab = "Odds ratio: forming a tie",
         transf = exp,
         refline = 1)
  
  dev.off()
  return(metareg)  
  
}




forestplot.half <- function(indata = NULL,
                       coefrow = NULL ,
                       variable = "",
                       varname = "",
                       conf = 95
){
  ##
  coef.table <- matrix(NA, nr=5, nc = 2) 
  for (i in 1:length(indata)){
    #Fill the table with varname row 1st col coef  
    coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable==varname)]
    #Fill the table with varname row 2nd col SE  
    coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable==varname)]
  }
  
  colnames(coef.table) <- c("coef","se")
  
  coef.table <- coef.table[!is.na(coef.table[,1]),] 
  #Run meta analysis
  metareg <- rma(yi=coef.table[,1],
                 sei=coef.table[,2],
                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline")
                 , method = "REML", level = conf)
  
  #Plot results
  setwd("\\\\192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")
  
  pdf(paste0(variable," forest 12 networks with outfrnds 7200 standardised new vars.pdf"))
  
  forest(metareg, 
         main = paste0("Tie probability by difference in ",variable),
         xlab = "Odds ratio: forming a tie",
         transf = exp,
         refline = 1)
  
  dev.off()
  return(metareg)  
  
}



#########################
#                       #
#  Main body of script  #
#                       #
#########################

##################################################

#                     6515 Models                #


load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/baseline_pooled_6200_sch1.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/baseline_pooled_6200_sch2.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/baseline_pooled_6200_sch3.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/baseline_pooled_6200_sch4.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/baseline_pooled_6200_sch5.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/baseline_pooled_6200_sch6.rdata")

load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/control_pooled_6200_sch1.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/control_pooled_6200_sch2.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/control_pooled_6200_sch3.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/control_pooled_6200_sch4.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/control_pooled_6200_sch5.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/HPC/7200/control_pooled_6200_sch6.rdata")


#indata <- list() 

#setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data")
#filename <- list.files(pattern = "ergm_6200_*")

#for (sch in 1:12) {
#  load(filename[sch])
#}


all.data <- list()
all.data[[1]] <- baseline.pooled.6200.sch1[[3]]
all.data[[2]] <- baseline.pooled.6200.sch2[[3]]
all.data[[3]] <- baseline.pooled.6200.sch3[[3]]


#####Omit School 4
#indata[[4]] <- baseline.pooled.6200.sch4[[3]]
#######Sex2 coef not defined. Fix to zero for purpose of plot
#indata[[4]]$pool.est[7] <- 0.00000000001
#indata[[4]]$pool.se[7]  <-  0.9
####Also se of ifactor for sexvar
#indata[[4]]$pool.se[12] <- 0.9

all.data[[4]] <- baseline.pooled.6200.sch5[[3]]
all.data[[5]] <- baseline.pooled.6200.sch6[[3]]

all.data[[6]]  <- control.pooled.6200.sch1[[3]]
all.data[[7]]  <- control.pooled.6200.sch2[[3]]
all.data[[8]]  <- control.pooled.6200.sch3[[3]]
####Omit school 4
#indata[[10]] <- control.pooled.6200.sch4[[3]]

all.data[[9]] <- control.pooled.6200.sch5[[3]]
all.data[[10]] <- control.pooled.6200.sch6[[3]]


#################################Sex 4 match variable not converging due to low numbers 

#Also, think baseline and control are labelled wrong. Double check

# create a new indata file that focuses only on converging schoools for talkvar4

indata.talk4 <- list()
indata.talk4[[1]] <- baseline.pooled.6200.sch1[[3]]
indata.talk4[[2]] <- baseline.pooled.6200.sch2[[3]]
indata.talk4[[3]] <- baseline.pooled.6200.sch3[[3]]
indata.talk4[[4]] <- baseline.pooled.6200.sch5[[3]]
indata.talk4[[5]] <- baseline.pooled.6200.sch6[[3]]


##############
### To use forestplot
#    Variable is the name of the file and title of plot
#    Varname must match that in the indata output
#    Conf is confidence intervals for the plot 
#    It saves a pdf plot in the control schools folder
#    And outputs a metaregression with control school dummy to the console

know.meta <- forestplot(indata =  all.data ,variable = "7200 knowledge"  , varname = "absdiff.know.var", conf = 95)
att.meta  <- forestplot(indata =  all.data ,variable = "7200 attitudes"  , varname = "absdiff.att.var", conf = 95)
conf.meta <- forestplot(indata =  all.data ,variable = "7200 confidence"  , varname = "absdiff.conf.var", conf = 95)

sex1match.meta <- forestplot(indata =  all.data ,variable = "7200 matchsex1"  , varname = "nodematch.talk.var.1")
sex2match.meta <- forestplot(indata =  all.data ,variable = "7200 matchsex2"  , varname = "nodematch.talk.var.2")
sex3match.meta <- forestplot(indata =  all.data ,variable = "7200 matchsex3"  , varname = "nodematch.talk.var.3")

#check
sex4match.meta <- forestplot(indata =  all.data ,variable = "7200 matchsex4"  , varname = "nodematch.talk.var.4")

edges.meta <- forestplot(indata =  all.data ,variable = "7200 edges"  , varname = "edges")
mutual.meta <- forestplot(indata =  all.data ,variable = "7200 mutual"  , varname = "mutual")
gwesp.meta <- forestplot(indata =  all.data ,variable = "7200 gwesp"  , varname = "gwesp.fixed.0.25")
ideg.meta <- forestplot(indata =  all.data ,variable = "7200 idegree"  , varname = "idegree1.5")

gendermatch.meta <- forestplot(indata =  all.data ,variable = "7200 gender match"  , varname = "nodematch.gender")

sex2ifac.meta <- forestplot(indata =  all.data ,variable = "7200 ifactor sex2"  , varname = "nodeifactor.talk.var.2")
sex3ifac.meta <- forestplot(indata =  all.data ,variable = "7200 ifactor sex3"  , varname = "nodeifactor.talk.var.3")
######!  Add sex4
sex4ifac.meta <- forestplot(indata =  all.data ,variable = "7200 ifactor sex4"  , varname = "nodeifactor.talk.var.4")


sex2ofac.meta <- forestplot(indata =  all.data ,variable = "7200 ofactor sex2", varname = "nodeofactor.talk.var.2")
sex3ofac.meta <- forestplot(indata =  all.data ,variable = "7200 ofactor sex3"  , varname = "nodeofactor.talk.var.3")
######!*  Add sex4
sex4ofac.meta <- forestplot(indata =  all.data ,variable = "7200 ofactor sex4"  , varname = "nodeofactor.talk.var.4")


knowicov.meta <- forestplot(indata =  all.data ,variable = "7200 indeg Knowledge", varname = "nodeicov.know.var")
knowocov.meta <- forestplot(indata =  all.data ,variable = "7200 outdeg Knowledge", varname = "nodeocov.know.var")

atticov.meta <- forestplot(indata =  all.data ,variable = "7200 indeg Attitude", varname = "nodeicov.att.var")
attocov.meta <- forestplot(indata =  all.data ,variable = "7200 outdeg Attitude", varname = "nodeocov.att.var")

conficov.meta <- forestplot(indata =  all.data ,variable = "7200 indeg Confidence", varname = "nodeicov.conf.var")
confocov.meta <- forestplot(indata =  all.data ,variable = "7200 outdeg Confidence", varname = "nodeocov.conf.var")



varnamecol <- c("edges",
                "mutual",
                "Gwesp",
                "Indegree Sqrt",
                "Gender match",
                "Sexually inactive",
                "Some activity",
                "Oral sex",
                "Vaginal sex",
                "Knowledge difference",
                "Attitudes difference",
                "Confidence difference",
                "in some activity",
                "in oral",
                "in vaginal",
                "in Knowledge",
                "in Attitudes",
                "in Confidence",
                "out some activity",
                "out oral",
                "out vaginal",
                "out Knowledge",
                "out Attitudes",
                "out  Confidence")


#####layout of meta analysis summary table

# Variable labels 

##Intercept lci uci 

# I squared variability

meta.table <- data.frame(matrix(NA, nr = length(varnamecol), nc = 3))
colnames(meta.table) <- c("Variable","beta (95% CI)", "I Squared")
meta.table[,1] <- varnamecol
counter    <- 1

for (varlist in c("edges.meta", "mutual.meta", "gwesp.meta","ideg.meta",
                  "gendermatch.meta", "sex1match.meta", "sex2match.meta", "sex3match.meta","sex4match.meta",
                  "know.meta", "att.meta", "conf.meta", "sex2ifac.meta", "sex3ifac.meta", "sex4ifac.meta", "knowicov.meta","atticov.meta",
                  "conficov.meta", "sex2ofac.meta", "sex3ofac.meta","sex4ofac.meta", "knowocov.meta", "attocov.meta", "confocov.meta"
)){
  xp <-  predict(get(varlist), transf = exp, digits = 2)
  meta.table[counter,2] <- paste0(round(xp$pred,2)," (",round(xp$ci.lb,2),", ",round(xp$ci.ub,2),")")
  meta.table[counter,3] <- round(get(varlist)$I2,2)
  counter <- counter + 1
}



write.csv(meta.table, file = "8200 Metaanalysis 4 sex cat all schools.csv")





know.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsknowledge"  , varname = "absdiff.know.var", conf = 95)
att.meta  <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsattitudes"  , varname = "absdiff.att.var", conf = 95)
conf.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsconfidence"  , varname = "absdiff.conf.var", conf = 95)

sex1match.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsmatchsex1"  , varname = "nodematch.talk.var.1")
sex2match.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsmatchsex2"  , varname = "nodematch.talk.var.2")
sex3match.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsmatchsex3"  , varname = "nodematch.talk.var.3")
sex4match.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsmatchsex4"  , varname = "nodematch.talk.var.4")

edges.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsedges"  , varname = "edges")
mutual.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsmutual"  , varname = "mutual")
gwesp.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsgwesp"  , varname = "gwesp.fixed.0.25")
ideg.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsidegree"  , varname = "idegree1.5")

gendermatch.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsgender match"  , varname = "nodematch.gender")

sex2ifac.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsifactor sex2"  , varname = "nodeifactor.talk.var.2")
sex3ifac.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsifactor sex3"  , varname = "nodeifactor.talk.var.3")
sex4ifac.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsifactor sex4"  , varname = "nodeifactor.talk.var.4")


sex2ofac.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsofactor sex2", varname = "nodeofactor.talk.var.2")
sex3ofac.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsofactor sex3"  , varname = "nodeofactor.talk.var.3")
sex4ofac.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsofactor sex4"  , varname = "nodeofactor.talk.var.4")


knowicov.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsindeg Knowledge", varname = "nodeicov.know.var")
knowocov.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsoutdeg Knowledge", varname = "nodeocov.know.var")

atticov.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsindeg Attitude", varname = "nodeicov.att.var")
attocov.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsoutdeg Attitude", varname = "nodeocov.att.var")

conficov.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsindeg Confidence", varname = "nodeicov.conf.var")
confocov.meta <- forestplot.half(indata =  indata.talk4 ,variable = "7200 half schoolsoutdeg Confidence", varname = "nodeocov.conf.var")



varnamecol <- c("edges",
                "mutual",
                "Gwesp",
                "Indegree Sqrt",
                "Gender match",
                "Sexually inactive",
                "Some activity",
                "Oral sex",
                "Vaginal sex",
                "Knowledge difference",
                "Attitudes difference",
                "Confidence difference",
                "in some activity",
                "in oral",
                "in vaginal",
                "in Knowledge",
                "in Attitudes",
                "in Confidence",
                "out some activity",
                "out oral",
                "out vaginal",
                "out Knowledge",
                "out Attitudes",
                "out  Confidence")


#####layout of meta analysis summary table

# Variable labels 

##Intercept lci uci 

# I squared variability

meta.table <- data.frame(matrix(NA, nr = length(varnamecol), nc = 3))
colnames(meta.table) <- c("Variable","beta (95% CI)", "I Squared")
meta.table[,1] <- varnamecol
counter    <- 1

for (varlist in c("edges.meta", "mutual.meta", "gwesp.meta","ideg.meta",
                  "gendermatch.meta", "sex1match.meta", "sex2match.meta", "sex3match.meta","sex4match.meta",
                  "know.meta", "att.meta", "conf.meta", "sex2ifac.meta", "sex3ifac.meta", "sex4ifac.meta", "knowicov.meta","atticov.meta",
                  "conficov.meta", "sex2ofac.meta", "sex3ofac.meta","sex4ofac.meta", "knowocov.meta", "attocov.meta", "confocov.meta"
)){
  xp <-  predict(get(varlist), transf = exp, digits = 2)
  meta.table[counter,2] <- paste0(round(xp$pred,2)," (",round(xp$ci.lb,2),", ",round(xp$ci.ub,2),")")
  meta.table[counter,3] <- round(get(varlist)$I2,2)
  counter <- counter + 1
}



write.csv(meta.table, file = "8200 Metaanalysis 4 sex cat half schools.csv")

