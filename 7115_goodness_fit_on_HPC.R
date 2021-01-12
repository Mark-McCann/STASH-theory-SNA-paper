rm(list = ls())

iternums <- 1:20


#################
#               #
#      Name     #
#               #
#################

# 7000 rubins pools

# Emily Long developed script for PaLS 

# Mark McCann modified for STASH


#############
#  Purpose  #
#############


# Pooling results of MI models in 6000

##############
#            #
#    Notes   #
#            #
##############

# 

#########################
#                       #
#  Outstanding actions  #
#                       #
#########################

####Update rubins.pools function for 6115 model coefs



#########################
#                       #
#    Load packages      #
#                       #
#########################

library(ergm)
library(mice)

#########################
#                       #
#     Load functions    #
#                       #
#########################
####################################### 
#   Pool results using Rubin's rules  #
#######################################

pool.6115 <- function(input.results = NULL, schoolnum = NULL) {
  leng <- length(input.results)
  #Two tables for coefs and se
  mice.coef.table <- matrix(0, nc = leng + 1, nr = length(input.results[[1]][[1]]$coef))
  mice.se.table <- mice.coef.table
  #Table for results of rubin's rules pool
  mice.pooled.est     <- data.frame(matrix(NA, nr = length(input.results[[1]][[1]]$coef), nc = 7))
  colnames(mice.pooled.est) <- c("variable","pool.est","pool.se","btw.var", "est/se","l.cl","u.cl")
  #Put names in the coef table
  for (x in 1:length(input.results[[1]][[1]]$coef) ){
    mice.pooled.est[x,1] <- names(input.results[[1]][[1]]$coef)[x]
  }  

  for (i in 1:leng) {
    col.id <- i + 1
    mice.coef.table[1,col.id] <- input.results[[i]][[schoolnum]]$coef['edges'] 
    mice.coef.table[2,col.id] <- input.results[[i]][[schoolnum]]$coef['mutual']
    mice.coef.table[3,col.id] <- input.results[[i]][[schoolnum]]$coef['gwesp.fixed.0.25']
    mice.coef.table[4,col.id] <- input.results[[i]][[schoolnum]]$coef['idegree1.5']
    mice.coef.table[5,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.gender']
    mice.coef.table[6,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.1']
    mice.coef.table[7,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.2']
    mice.coef.table[8,col.id] <- input.results[[i]][[schoolnum]]$coef['nodematch.sex.var.3']
    mice.coef.table[9,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.std.know']
    mice.coef.table[10,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.std.att']
    mice.coef.table[11,col.id] <- input.results[[i]][[schoolnum]]$coef['absdiff.std.conf']
    mice.coef.table[12,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeifactor.sex.var.2']
    mice.coef.table[13,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeifactor.sex.var.3']
    mice.coef.table[14,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeofactor.sex.var.2']
    mice.coef.table[15,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeofactor.sex.var.3']
    mice.coef.table[16,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeicov.std.know']
    mice.coef.table[17,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeicov.std.att']
    mice.coef.table[18,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeicov.std.conf']
    mice.coef.table[19,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeocov.std.know']
    mice.coef.table[20,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeocov.std.att']
    mice.coef.table[21,col.id] <- input.results[[i]][[schoolnum]]$coef['nodeocov.std.conf']
    
    mice.se.table[1,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['edges']
    mice.se.table[2,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['mutual']
    mice.se.table[3,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['gwesp.fixed.0.25']
    mice.se.table[4,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['idegree1.5']
    mice.se.table[5,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.gender']
    mice.se.table[6,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.1']
    mice.se.table[7,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.2']
    mice.se.table[8,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodematch.sex.var.3']
    mice.se.table[9,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.std.know']
    mice.se.table[10,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.std.att']
    mice.se.table[11,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['absdiff.std.conf']
    mice.se.table[12,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeifactor.sex.var.2']
    mice.se.table[13,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeifactor.sex.var.3']
    mice.se.table[14,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeofactor.sex.var.2']
    mice.se.table[15,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeofactor.sex.var.3']
    mice.se.table[16,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeicov.std.know']
    mice.se.table[17,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeicov.std.att']
    mice.se.table[18,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeicov.std.conf']
    mice.se.table[19,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeocov.std.know']
    mice.se.table[20,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeocov.std.att']
    mice.se.table[21,col.id] <- summary(input.results[[i]][[schoolnum]])$asyse['nodeocov.std.conf']
  }

  for (i in 1:dim(mice.coef.table)[1]) {
    pooled.res <- pool.scalar(mice.coef.table[i,2:(leng + 1)], mice.se.table[i,2:(leng + 1)],  k = 1) 
    mice.pooled.est[i,2] <-  round(pooled.res$qbar,2)
    mice.pooled.est[i,3] <-  round(pooled.res$ubar,2)
    mice.pooled.est[i,4] <-  round(pooled.res$b,2)
    mice.pooled.est[i,5] <-  round(pooled.res$qbar / pooled.res$ubar , 2)
    mice.pooled.est[i,6] <-  round(pooled.res$qbar - (1.96 * pooled.res$ubar),2)
    mice.pooled.est[i,7] <-  round(pooled.res$qbar + (1.96 * pooled.res$ubar),2)  
    
  }
  
  mice.array <- list(coefs = mice.coef.table, 
                     ses = mice.se.table, 
                     pooled.ests = mice.pooled.est)
  
  return(mice.array)
}


#########################
#                       #
#  Main body of script  #
#                       #
#########################


# Rubin's pools requires an input obect that has the indexing format:
#     object[[imputation]][schoolid]

####Load 6115 models
setwd("/export/home/mmc78h/STASH/")

######Check which models haven't completed

for (sch in c(1,2,3,5,6) ){
  for (iter in iternums){
    for (borc in c("baseline","control")){          
      if ( !file.exists(paste0("ergm_6115_imp",iter,"_sch_",sch,"_",borc,".rdata") ) ) print(paste0("Missing file iter",iter," sch",sch," ",borc)) 
    }
  }
}


###Create lists to hold results from all models
baseline.6115.models <- list()
control.6115.models <- list()

#Let these hold inner lists to hold iter and school indices
prelist <- list()
prelist[[1]] <- NA

baseline.6115.models[[max(iternums)]] <- prelist
control.6115.models[[max(iternums)]] <- prelist


for (sch in c(1,2,3,5,6) ){
  for (iter in iternums ){
    for (borc in c("baseline")){          
      load(paste0("ergm_6115_imp",iter,"_sch_",sch,"_",borc,".rdata") )
      baseline.6115.models[[iter]][[sch]] <- imputation.list[[iter]][[sch]]
      print(paste0("Imputation ",iter," School ",sch," loaded successfully"))
    }
  }
}

for (sch in c(1,2,3,5,6) ){
  for (iter in iternums ){
    for (borc in c("control")){          
      load(paste0("ergm_6115_imp",iter,"_sch_",sch,"_",borc,".rdata") )
      control.6115.models[[iter]][[sch]] <- imputation.list[[iter]][[sch]]
      print(paste0("Imputation ",iter," School ",sch," loaded successfully"))
    }
  }
}


for (i in c(1,2,3,5,6)){
  assign(paste0("baseline.pooled.6115.sch",i), pool.6115(input.results = baseline.6115.models, schoolnum = i))
  assign(paste0("control.pooled.6115.sch" ,i), pool.6115(input.results = control.6115.models, schoolnum = i))
}


#####Take model results and put into a csv for an article table
pooled.results.table <- cbind(baseline.pooled.6115.sch1[[3]]$variable,
                              ##Baseline coefs
                              paste0(baseline.pooled.6115.sch1[[3]]$pool.est," (",
                                     baseline.pooled.6115.sch1[[3]]$l.cl    ,", ",
                                     baseline.pooled.6115.sch1[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.6115.sch2[[3]]$pool.est," (",
                                     baseline.pooled.6115.sch2[[3]]$l.cl    ,", ",
                                     baseline.pooled.6115.sch2[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.6115.sch3[[3]]$pool.est," (",
                                     baseline.pooled.6115.sch3[[3]]$l.cl    ,", ",
                                     baseline.pooled.6115.sch3[[3]]$u.cl    ,")"),
#                              paste0(baseline.pooled.6115.sch4[[3]]$pool.est," (",
#                                     baseline.pooled.6115.sch4[[3]]$l.cl    ,", ",
 #                                    baseline.pooled.6115.sch4[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.6115.sch5[[3]]$pool.est," (",
                                     baseline.pooled.6115.sch5[[3]]$l.cl    ,", ",
                                     baseline.pooled.6115.sch5[[3]]$u.cl    ,")"),
                              paste0(baseline.pooled.6115.sch6[[3]]$pool.est," (",
                                     baseline.pooled.6115.sch6[[3]]$l.cl    ,", ",
                                     baseline.pooled.6115.sch6[[3]]$u.cl    ,")")
                              ,
                              ######Control schools
                              paste0(control.pooled.6115.sch1[[3]]$pool.est," (",
                                     control.pooled.6115.sch1[[3]]$l.cl    ,", ",
                                     control.pooled.6115.sch1[[3]]$u.cl    ,")"),
                              paste0(control.pooled.6115.sch2[[3]]$pool.est," (",
                                     control.pooled.6115.sch2[[3]]$l.cl    ,", ",
                                     control.pooled.6115.sch2[[3]]$u.cl    ,")"),
                              paste0(control.pooled.6115.sch3[[3]]$pool.est," (",
                                     control.pooled.6115.sch3[[3]]$l.cl    ,", ",
                                     control.pooled.6115.sch3[[3]]$u.cl    ,")"),
#                              paste0(control.pooled.6115.sch4[[3]]$pool.est," (",
#                                    control.pooled.6115.sch4[[3]]$l.cl    ,", ",
#                                    control.pooled.6115.sch4[[3]]$u.cl    ,")"),
                             paste0(control.pooled.6115.sch5[[3]]$pool.est," (",
                                    control.pooled.6115.sch5[[3]]$l.cl    ,", ",
                                    control.pooled.6115.sch5[[3]]$u.cl    ,")"),
                             paste0(control.pooled.6115.sch6[[3]]$pool.est," (",
                                    control.pooled.6115.sch6[[3]]$l.cl    ,", ",
                                    control.pooled.6115.sch6[[3]]$u.cl    ,")")
                             
                             
)

colnames(pooled.results.table) <- c("Parameter","School 1b","School 2b","School 3b",
                                    # "School 4b",
                                    "School 5b", "School 6b",
                                    "School 1c","School 2c","School 3c",
                                    #"School 4c",
                                    "School 5c", "School 6c")

#View(pooled.results.table)
write.csv(pooled.results.table, file = "pooled estimates_6115.csv")


for (sch in c(1,2,3,5,6)){
  save(list = paste0("baseline.pooled.6115.sch",sch), file = paste0("baseline_pooled_6115_sch",sch,".rdata") )
  save(list = paste0("control.pooled.6115.sch",sch), file = paste0("control_pooled_6115_sch",sch,".rdata") )
}



#############################################################################
######           Create pdfs of mcmc diagnostics and gof  #######
#############################################################################

#for (sch in 1){
#  pdf(paste0("mcmc_diag_baseline_sch",sch,".pdf"))
#    for (iter in iternums){    
#      mcmc.diagnostics(baseline.6115.models[[iter]][[sch]])
#      ideg <- gof(baseline.6115.models[[iter]][[sch]]~idegree, control.gof.ergm(parallel=16, parallel.type="MPI"))
#      plot(ideg)
#      odeg <- gof(baseline.6115.models[[iter]][[sch]]~odegree, control.gof.ergm(parallel=16, parallel.type="MPI"))
#      plot(odeg)
#      tricen <- gof(baseline.6115.models[[iter]][[sch]]~triadcensus, control.gof.ergm(parallel=16, parallel.type="MPI"))
#      plot(tricen)
#    }
#  dev.off()
#}


for (sch in c(1,2,3,5,6)){
  pdf(paste0("mcmc_diag_control_sch",sch,".pdf"))
    for (iter in iternums){    
      mcmc.diagnostics(control.6115.models[[iter]][[sch]])
      ideg <- gof(control.6115.models[[iter]][[sch]]~idegree, control.gof.ergm(parallel=16, parallel.type="MPI"))
      plot(ideg)
      odeg <- gof(control.6115.models[[iter]][[sch]]~odegree, control.gof.ergm(parallel=16, parallel.type="MPI"))
      plot(odeg)
      tricen <- gof(control.6115.models[[iter]][[sch]]~triadcensus, control.gof.ergm(parallel=16, parallel.type="MPI"))
      plot(tricen)
    }
  dev.off()
}

