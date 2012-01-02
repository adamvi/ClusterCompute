######################################################################################################################################################
### killCluster - terminates all running instances
###	User either has to log out of Master node to execute or start another instance of R, change working directory, etc. and execute

`killCluster` <-
function(trash=FALSE) {
	# Get instance id's from running instances (from file or by using ec2 Tools)
	options(warn=-1)
	insts<-NULL
	try(insts<-unserialize(charToRaw(readChar("CLUSTER_CONFIG_FILES/runningInstances.txt", 
		file.info("CLUSTER_CONFIG_FILES/runningInstances.txt")$size))), silent = TRUE)
	options(warn=0)
	
	if (is.null(insts)) {
		instancesParsed <- sapply(system("ec2-describe-instances", intern=TRUE), strsplit, "\t")
		ri <- 1
		for (i in 1:length(instancesParsed)) {
			if (instancesParsed[[i]][[1]]=="INSTANCE") {
				if (instancesParsed[[i]][[6]]=="running") {
					insts[[ri]] <- instancesParsed[[i]]
					ri <- ri + 1
				}
			}
		}
	}

	for (b in 1:length(insts)) eval(parse(text=paste("system(\"ec2-terminate-instances ", insts[[b]][[2]], "\")", sep = "")))
	
	# Delete the temporary Cluster Configuration files if TRUE
	if (trash) {
		unlink("CLUSTER_CONFIG_FILES", recursive=TRUE)
		unlink("*_CMD_*.Rout")
		unlink("*_CMD*.Rout")
		unlink("singleNode_*.Rout")
	}
} # END killCluster function