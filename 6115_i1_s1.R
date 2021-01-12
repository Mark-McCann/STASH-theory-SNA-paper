rm(list = ls())

#This version is for each separate hpc run
version <- 1
schoolid <- 1
#################
#               #
#      Name     #
#               #
#################

# 6000 Ergms

# Chiara Broccatelli developed ergm script
# Mark McCann modified to loop over imputations


#############
#  Purpose  #
#############

# Running ergms across imp samples

##############
#            #
#    Notes   #
#            #
##############

# 

######################### 

#  Outstanding actions  #
#                       #
######################### 


######################### 

#    Load packages      #
#                       #
######################### 


require("Rmpi")
require("backports")
require("crayon")
require("vctrs")
require("pillar")
require("network")
require("ergm")
######################### 

#     Load functions    # 

#                       #
######################### 

run.imputed.ergms <- function(dataset = NULL, savename = NULL, num_iters = NA, sch = NA) {
  
  
    for (j in num_iters) {
      df1 <- dataset[[j]]
        for (i in sch) {
          df <- df1[[i]]
          school.result.list[[i]] <- ergm(df ~ edges 
                                      + mutual
                                      + gwesp(0.25, fixed = T)
                                      + idegree1.5()
                                      + nodematch("gender")
                                      + nodematch("sex.var", diff = T)
                                      + absdiff("std.know")
                                      + absdiff("std.att")
                                      + absdiff("std.conf")
                                      + nodeifactor("sex.var")
                                      + nodeofactor("sex.var")
                                      + nodeicov("std.know")
                                      + nodeicov("std.att")
                                      + nodeicov("std.conf")
                                      + nodeocov("std.know")
                                      + nodeocov("std.att")
                                      + nodeocov("std.conf")
                                      ,
                                      directed=T, 
                                      constraints=~bd(maxout=6),
                                      control=control.ergm(main.method=c("MCMLE"),
									                       MCMC.burnin = 200000, MCMC.interval=4000, 
                                                           force.main=F , seed = 274,
                                                           parallel=16, parallel.type="MPI"),
                                      eval.loglik = F ###This cuts computation time but doesnt update screen
      )
    }
    imputation.list[[j]] <- school.result.list
    print(j)
 
    save(imputation.list, file = paste0("ergm_6115_imp",num_iters,"_sch_",sch,"_",savename,".rdata"))
    
  }
  
  return(imputation.list)
  
}

######################### 

#  Main body of script  #
#                       #
######################### 

setwd("/export/home/mmc78h/STASH/")


load("ergm_data_control_imputed.rdata")
load("ergm_data_baseline_imputed.rdata")

#####Run these before calling the run.imputed.ergms function     
imputation.list <- list()
school.result.list <- list()

#####Baseline completed up to 22 imputations
baseline.ideg.odeg <- run.imputed.ergms(dataset = ergm.data.baseline.imputed, 
                                          savename = "baseline",
                                          num_iters = version,
										  sch = schoolid)

control.ideg.odeg <- run.imputed.ergms(dataset = ergm.data.control.imputed, 
                                         savename = "control",
                                         num_iters = version,
										  sch = schoolid)










