rm(list = ls())
#################
#               #
#      Name     #
#               #
#################

# 8000 metafor combine models 

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

#########################

forestplot <- function(data = NULL,
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
#  setwd("\\\\192.168.0.17/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")
  setwd("//med2023.campus.gla.ac.uk/stash_sna/DisseminationAndImpact/Manuscripts_Papers/Control Schools Paper")
  
  
  
  pdf(paste0(variable," forest 12 networks with outfrnds 6115 standardised new vars.pdf"))
  
  forest(metareg, 
         main = paste0("Tie probability by difference in ",variable),
         xlab = "Odds ratio: forming a tie",
         transf = exp,
         refline = 1)
  
  dev.off()
  return(metareg)  
  
}
#########################

#########################
#                       #
#  Main body of script  #
#                       #
#########################

#########################
#   Load 6115 Models    #
#########################


# Load data
#########################
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch1.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch2.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch3.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch4.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch5.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch6.rdata")

load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch1.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch2.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch3.rdata")
#load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch4.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch5.rdata")
load("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch6.rdata")
#########################

# Load data off campus

#########################
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch1.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch2.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch3.rdata")
#load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch4.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch5.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/baseline_pooled_6115_sch6.rdata")

load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch1.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch2.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch3.rdata")
#load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch4.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch5.rdata")
load("//med2023.campus.gla.ac.uk/stash_sna/Data/AnonymisedData/working data/control_pooled_6115_sch6.rdata")
#########################


#########################
## Load the files 
##  into a list 
##   for metaanalysis
#########################

#########################
indata <- list()
indata[[1]] <- baseline.pooled.6115.sch1[[3]]
indata[[2]] <- baseline.pooled.6115.sch2[[3]]
indata[[3]] <- baseline.pooled.6115.sch3[[3]]

#####Omit School 4
#indata[[4]] <- baseline.pooled.6115.sch4[[3]]
#######Sex2 coef not defined. Fix to zero for purpose of plot
#indata[[4]]$pool.est[7] <- 0.00000000001
#indata[[4]]$pool.se[7]  <-  0.9
####Also se of ifactor for sexvar
#indata[[4]]$pool.se[12] <- 0.9

indata[[4]] <- baseline.pooled.6115.sch5[[3]]
indata[[5]] <- baseline.pooled.6115.sch6[[3]]

indata[[6]]  <- control.pooled.6115.sch1[[3]]
indata[[7]]  <- control.pooled.6115.sch2[[3]]
indata[[8]]  <- control.pooled.6115.sch3[[3]]
####Omit school 4
#indata[[10]] <- control.pooled.6115.sch4[[3]]

indata[[9]] <- control.pooled.6115.sch5[[3]]
indata[[10]] <- control.pooled.6115.sch6[[3]]
#########################


##############
### To use forestplot
#    Variable is the name of the file and title of plot
#    Varname must match that in the indata output
#    Conf is confidence intervals for the plot 
#    It saves a pdf plot in the control schools folder
#    And outputs a metaregression with control school dummy to the console

#########################
know.meta <- forestplot(data =  indata ,variable = "6115 knowledge"  , varname = "absdiff.std.know", conf = 95)
att.meta  <- forestplot(data =  indata ,variable = "6115 attitudes"  , varname = "absdiff.std.att", conf = 95)
conf.meta <- forestplot(data =  indata ,variable = "6115 confidence"  , varname = "absdiff.std.conf", conf = 95)

sex1match.meta <- forestplot(data =  indata ,variable = "6115 matchsex1"  , varname = "nodematch.sex.var.1")
sex2match.meta <- forestplot(data =  indata ,variable = "6115 matchsex2"  , varname = "nodematch.sex.var.2")
sex3match.meta <- forestplot(data =  indata ,variable = "6115 matchsex3"  , varname = "nodematch.sex.var.3")

edges.meta <- forestplot(data =  indata ,variable = "6115 edges"  , varname = "edges")
mutual.meta <- forestplot(data =  indata ,variable = "6115 mutual"  , varname = "mutual")
gwesp.meta <- forestplot(data =  indata ,variable = "6115 gwesp"  , varname = "gwesp.fixed.0.25")
ideg.meta <- forestplot(data =  indata ,variable = "6115 idegree"  , varname = "idegree1.5")

gendermatch.meta <- forestplot(data =  indata ,variable = "6115 gender match"  , varname = "nodematch.gender")

sex2ifac.meta <- forestplot(data =  indata ,variable = "6115 ifactor sex2"  , varname = "nodeifactor.sex.var.2")
sex3ifac.meta <- forestplot(data =  indata ,variable = "6115 ifactor sex3"  , varname = "nodeifactor.sex.var.3")
sex2ofac.meta <- forestplot(data =  indata ,variable = "6115 ofactor sex2", varname = "nodeofactor.sex.var.2")
sex3ofac.meta <- forestplot(data =  indata ,variable = "6115 ofactor sex3"  , varname = "nodeofactor.sex.var.3")
knowicov.meta <- forestplot(data =  indata ,variable = "6115 indeg Knowledge", varname = "nodeicov.std.know")
knowocov.meta <- forestplot(data =  indata ,variable = "6115 outdeg Knowledge", varname = "nodeocov.std.know")

atticov.meta <- forestplot(data =  indata ,variable = "6115 indeg Attitude", varname = "nodeicov.std.att")
attocov.meta <- forestplot(data =  indata ,variable = "6115 outdeg Attitude", varname = "nodeocov.std.att")

conficov.meta <- forestplot(data =  indata ,variable = "6115 indeg Confidence", varname = "nodeicov.std.conf")
confocov.meta <- forestplot(data =  indata ,variable = "6115 outdeg Confidence", varname = "nodeocov.std.conf")
#########################


####################################
##   meta analysis summary table  ##
####################################

#########################
varnamecol <- c("edges",
                "mutual",
                "Gwesp",
                "Indegree Sqrt",
                "Gender match",
                "Not sexually active",
                "Active no intercourse",
                "Intercourse",
                "Knowledge difference",
                "Attitudes difference",
                "Confidence difference",
                "in Active no intercourse",
                "in Intercourse",
                "in Knowledge",
                "in Attitudes",
                "in Confidence",
                "out Active no intercourse",
                "out Intercourse",
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
                  "gendermatch.meta", "sex1match.meta", "sex2match.meta", "sex3match.meta",
                  "know.meta", "att.meta", "conf.meta", "sex2ifac.meta", "sex3ifac.meta", "knowicov.meta","atticov.meta",
                  "conficov.meta", "sex2ofac.meta", "sex3ofac.meta", "knowocov.meta", "attocov.meta", "confocov.meta"
                  )){
  xp <-  predict(get(varlist), transf = exp, digits = 2)
  meta.table[counter,2] <- paste0(round(xp$pred,2)," (",round(xp$ci.lb,2),", ",round(xp$ci.ub,2),")")
  meta.table[counter,3] <- round(get(varlist)$I2,2)
  counter <- counter + 1
}


write.csv(meta.table, file = "Metaanalysis table 10 schools 20 imputations omit sch 4 6115 new standardised vars.csv")
#########################


################################################################
################################################################
########    Combined Plots for Article 
################################################################
################################################################


###########################
##  Three sex var plots  ##
###########################

###########################
#Create a 12 * 3 table, 12 schools in rows, coefficient, SE, and baseline/control in columns

inact.coef.table <- matrix(NA, nr=10, nc = 3) 
  for (i in 1:length(indata)){
    #Fill the table with varname row 1st col coef  
    inact.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="nodematch.sex.var.1")]
    #Fill the table with varname row 2nd col SE  
    inact.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="nodematch.sex.var.1")]
  }

mid.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  mid.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="nodematch.sex.var.2")]
  #Fill the table with varname row 2nd col SE  
  mid.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="nodematch.sex.var.2")]
}

orvag.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  orvag.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="nodematch.sex.var.3")]
  #Fill the table with varname row 2nd col SE  
  orvag.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="nodematch.sex.var.3")]
}


#Run meta analysis
inact.meta <- rma(yi=inact.coef.table[,1],
                 sei=inact.coef.table[,2],
                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                        , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                 , method = "REML", level = 95)


mid.meta <- rma(yi=mid.coef.table[,1],
                  sei=mid.coef.table[,2],
 #                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
#                         , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                   method = "REML", level = 95)


orvag.meta <- rma(yi=orvag.coef.table[,1],
                  sei=orvag.coef.table[,2],
#                  slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
#                       , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                  method = "REML", level = 95)


full.coef.table <- rbind(inact.coef.table,
                         mid.coef.table,
                         orvag.coef.table)

full.coef.table[,3] <- 1:30

all.meta <- rma(yi   =full.coef.table[,1],
                  sei=full.coef.table[,2],
                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline",
                       "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control",
                       "Sch 1 baseline ", "Sch 2 baseline ", "Sch 3 baseline ", "Sch 5 baseline ", "Sch 6 baseline ",
                        "Sch 1 control ", "Sch 2 control ","Sch 3 control ","Sch 5 control ","Sch 6 control ",
                       "Sch 1 baseline  ", "Sch 2 baseline  ", "Sch 3 baseline  ", "Sch 5 baseline  ", "Sch 6 baseline  ",
                       "Sch 1 control  ", "Sch 2 control  ","Sch 3 control  ","Sch 5 control  ","Sch 6 control  ")
                              ,
                  method = "REML", level = 95)



bmp("Sexual Activity combined Forest plot.bmp", width = 1200 , height = 1200)

forest(all.meta,  main = "",
       xlab = "Odds ratio: forming a tie",
       transf = exp,
       refline = 1,
       rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
       top = 1, # How many blank rows at top of plot for a header
       cex = 1.25, 
       ylim = c(1,37),
       xlim = c(-2,6),  # Limit of whole plot
       alim = c(-0.5,4.5), # Limit of forest axes
       at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
       addfit = FALSE)

### switch to bold font
par(font=2)

text(-2, c(36), pos=4, font = 2, cex=1.25, c("Oral/Vaginal"))
addpoly(orvag.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
text(-2, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(orvag.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(-2, c(24), pos=4, cex=1.25, c("Active no sex"))
addpoly(mid.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
text(-2, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(mid.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(-2, c(12), pos=4, cex=1.25, c("Sexually inactive"))
addpoly(inact.meta, row= 1, cex=1.25,  mlab="", transf = exp)
text(-2, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(inact.meta$I2, digits=1, format="f"), "%"))
dev.off()

###########################


tiff("Sexual Activity combined Forest plot.tif", width = 1200 , height = 1200)

forest(all.meta,  main = "",
       xlab = "Odds ratio: forming a tie",
       transf = exp,
       refline = 1,
       rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
       top = 1, # How many blank rows at top of plot for a header
       cex = 1.25, 
       ylim = c(1,37),
       xlim = c(-2,6),  # Limit of whole plot
       alim = c(-0.5,4.5), # Limit of forest axes
       at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
       addfit = FALSE)

### switch to bold font
par(font=2)

text(-2, c(36), pos=4, font = 2, cex=1.25, c("Oral/Vaginal"))
addpoly(orvag.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
text(-2, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(orvag.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(-2, c(24), pos=4, cex=1.25, c("Active no sex"))
addpoly(mid.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
text(-2, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(mid.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(-2, c(12), pos=4, cex=1.25, c("Sexually inactive"))
addpoly(inact.meta, row= 1, cex=1.25,  mlab="", transf = exp)
text(-2, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(inact.meta$I2, digits=1, format="f"), "%"))
dev.off()

###########################



#############################
#### Know att conf plots ####
#############################


###########################
#Create a 12 * 3 table, 12 schools in rows, coefficient, SE, and baseline/control in columns

know.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  know.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="absdiff.std.know")]
  #Fill the table with varname row 2nd col SE  
  know.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="absdiff.std.know")]
}

att.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  att.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="absdiff.std.att")]
  #Fill the table with varname row 2nd col SE  
  att.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="absdiff.std.att")]
}

conf.coef.table <- matrix(NA, nr=10, nc = 3) 
for (i in 1:length(indata)){
  #Fill the table with varname row 1st col coef  
  conf.coef.table[i,1] <- indata[[i]]$pool.est[which(indata[[i]]$variable=="absdiff.std.conf")]
  #Fill the table with varname row 2nd col SE  
  conf.coef.table[i,2] <- indata[[i]]$pool.se[which(indata[[i]]$variable=="absdiff.std.conf")]
}


#Run meta analysis
know.meta <- rma(yi=know.coef.table[,1],
                  sei=know.coef.table[,2],
                  slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                         , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                  , method = "REML", level = 95)


att.meta <- rma(yi=att.coef.table[,1],
                sei=att.coef.table[,2],
                #                 slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                #                         , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                method = "REML", level = 95)


conf.meta <- rma(yi=conf.coef.table[,1],
                  sei=conf.coef.table[,2],
                  #                  slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline"
                  #                       , "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control") 
                  method = "REML", level = 95)


full.coef.table <- rbind(know.coef.table,
                         att.coef.table,
                         conf.coef.table)

full.coef.table[,3] <- 1:30

all.meta <- rma(yi   =full.coef.table[,1],
                sei=full.coef.table[,2],
                slab=c("Sch 1 baseline", "Sch 2 baseline", "Sch 3 baseline", "Sch 5 baseline", "Sch 6 baseline",
                       "Sch 1 control", "Sch 2 control","Sch 3 control","Sch 5 control","Sch 6 control",
                       "Sch 1 baseline ", "Sch 2 baseline ", "Sch 3 baseline ", "Sch 5 baseline ", "Sch 6 baseline ",
                       "Sch 1 control ", "Sch 2 control ","Sch 3 control ","Sch 5 control ","Sch 6 control ",
                       "Sch 1 baseline  ", "Sch 2 baseline  ", "Sch 3 baseline  ", "Sch 5 baseline  ", "Sch 6 baseline  ",
                       "Sch 1 control  ", "Sch 2 control  ","Sch 3 control  ","Sch 5 control  ","Sch 6 control  ")
                ,
                method = "REML", level = 95)



bmp("Know Att Conf combined Forest plot.bmp", width = 1200 , height = 1200)

forest(all.meta,  main = "",
       xlab = "Odds ratio: forming a tie",
       transf = exp,
       refline = 1,
       rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
       top = 1, # How many blank rows at top of plot for a header
       cex = 1.25, 
       ylim = c(1,37),
       xlim = c(0.5,1.5),  # Limit of whole plot
#       alim = c(-0.5,4.5), # Limit of forest axes
#       at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
       addfit = FALSE)

### switch to bold font
par(font=2)

text(0.5, c(36), pos=4, font = 2, cex=1.25, c("Confidence"))
addpoly(conf.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
text(0.5, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(conf.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(0.5, c(24), pos=4, cex=1.25, c("Norms"))
addpoly(att.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
text(0.5, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(att.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(0.5, c(12), pos=4, cex=1.25, c("Knowledge"))
addpoly(know.meta, row= 1, cex=1.25,  mlab="", transf = exp)
text(0.5, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(know.meta$I2, digits=1, format="f"), "%"))
dev.off()

###########################


tiff("Know Att Conf combined Forest plot.tif", width = 1200 , height = 1200)

forest(all.meta,  main = "",
       xlab = "Odds ratio: forming a tie",
       transf = exp,
       refline = 1,
       rows=c(2:11,14:23,26:35), # Which rows to put estimates - leave space for the subgroup diamonds and titles
       top = 1, # How many blank rows at top of plot for a header
       cex = 1.25, 
       ylim = c(1,37),
       xlim = c(0.5,1.5),  # Limit of whole plot
       #       alim = c(-0.5,4.5), # Limit of forest axes
       #       at = c(0,0.5,1,2,3.25,4), # Points on forest x axis
       addfit = FALSE)

### switch to bold font
par(font=2)

text(0.5, c(36), pos=4, font = 2, cex=1.25, c("Confidence"))
addpoly(conf.meta, row= 25 , cex=1.25,  mlab="", transf = exp)
text(0.5, 25, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(conf.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(0.5, c(24), pos=4, cex=1.25, c("Norms"))
addpoly(att.meta  , row= 13, cex=1.25,  mlab="", transf = exp)
text(0.5, 13, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(att.meta$I2, digits=1, format="f"), "%"))

par(font=2)
text(0.5, c(12), pos=4, cex=1.25, c("Knowledge"))
addpoly(know.meta, row= 1, cex=1.25,  mlab="", transf = exp)
text(0.5, 1, pos=4, font = 4, cex=1.25, paste("RE Model. I^2 = ",formatC(know.meta$I2, digits=1, format="f"), "%"))
dev.off()

###########################

