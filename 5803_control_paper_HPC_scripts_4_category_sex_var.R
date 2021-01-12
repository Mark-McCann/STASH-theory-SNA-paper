#################
#               #
#      Name     #
#               #
#################

# Mark McCann created the file

#############
#  Purpose  #
#############

# Creates separate hpc files - modified to use standardised variables

##############
#            #
#    Notes   #
#            #
##############


#########################
#                       #
#    Load packages      #
#                       #
#########################

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/hpc_script_prep_may_2020")

#######HPC R files
#read files in the directory

###Work off imp1 s1 and then replicate the modifications

#Load in file 1
    for (sch in 1){      
      for (imp in 1){
        file <- readLines(paste0("6100_i",imp,"_s",sch,".R") )
      }}


###Change the  sex.var to the 4 category version - called talk.var 
file[[72]] <- "                                      + nodematch(\"talk.var\", diff = T)"
file[[76]] <- "                                      + nodeifactor(\"talk.var\")"
file[[77]] <- "                                      + nodeofactor(\"talk.var\")"

file[[97]] <- "    save(imputation.list, file = paste0(\"ergm_6200_imp\",num_iters,\"_sch_\",sch,\"_\",savename,\".rdata\"))"

file[[132]] <- ""
file[[133]] <- ""
file[[139]] <- ""

#loop over school and imputation and save file
    
    for (sch in 1:6){      
      for (imp in 1:20){
        
        file[4] <- paste0("version <- ",imp)
        file[5] <- paste0("schoolid <- ",sch)
        writeLines(file[1:140], con = paste0("6200_i",imp,"_s",sch,".R"))
    }
  }


####Read in a batch file
####Change the batch file to refer to correct school and imputation
for (sch in 1:6){      
  for (imp in 1:20){
    setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/hpc_script_prep")
    infile <- readLines(paste0("6100_i",imp,"_s",sch,".sh" ))
    infile[11] <- paste0(" R CMD BATCH /export/home/mmc78h/STASH/6200_i",imp,"_s",sch,".R /export/home/mmc78h/STASH/6200_i",imp,"_s",sch,".out")
    setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/hpc_script_prep_may_2020")
    writeLines(infile, con = paste0("6200_i",imp,"_s",sch,".sh"))
  }
}

  ####Change to run on centos7
    for (sch in 1:6){      
      for (imp in 1:20){
        infile <- readLines(paste0("6200_i",imp,"_s",sch,".sh" ))
        infile[2] <- "#PBS -l nodes=1:ppn=16:centos7"
        infile[8] <- "export LD_LIBRARY_PATH=/usr/lib64/openmpi/lib:$LD_LIBRARY_PATH"
        infile[11] <- paste0(" R CMD BATCH /export/home/mmc78h/STASH/6200_i",imp,"_s",sch,".R /export/home/mmc78h/STASH/6200_i",imp,"_s",sch,".out")
        writeLines(infile, con = paste0("6200_i",imp,"_s",sch,".sh"))
      }
    }
    
    
    
#Create list of run commands for copying and pasting into putty

setwd("//192.168.0.17/stash_sna/Data/AnonymisedData/working data/hpc_script_prep_may_2020")
counter <- 1
pastefile <- vector("character",length = 120)
for (sch in 1:6){      
  for (imp in 1:20){
    
    pastefile[counter] <- paste0("qsub 6200_i",imp,"_s",sch,".sh")
    counter <- counter + 1
  }
}

writeLines(pastefile, con = "6200_HPC_copypaste_commands_into_putty.txt")
