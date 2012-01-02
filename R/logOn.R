######################################################################################################################################################
### logOn - Log on to the Master or Worker Node from user's local machine.
###	
###	User either has to log out of Master node to execute or execute from alternate (local) terminal instance of R

`logOn`<- 
function(node.num=0, ec2.path.override) {
# Get the clusterConfig call to use arguments
	call<-as.call(unserialize(charToRaw(readChar("CLUSTER_CONFIG_FILES/call.txt", file.info("CLUSTER_CONFIG_FILES/call.txt")$size))))
	 
 # Get instance id's from running instances (from file or by using ec2 Tools)
	options(warn=-1)
	insts<-NULL
	try(insts<-unserialize(charToRaw(readChar("CLUSTER_CONFIG_FILES/runningInstances.txt", 
		file.info("CLUSTER_CONFIG_FILES/runningInstances.txt")$size))), silent = TRUE)
	options(warn=0)
	
	if (is.null(insts)) {
		instancesParsed <- sapply(system("ec2-describe-instances", intern=TRUE), strsplit, "\t")
		ri <- 1
		for (i in 1:length(instancesParsed)){
			if (instancesParsed[[i]][[1]]=="INSTANCE") {
				if (instancesParsed[[i]][[6]]=="running") {
					insts[[ri]] <- instancesParsed[[i]]
					ri <- ri + 1
				}
			}
		}
	}
	
	if(!missing(ec2.path.override)) {
		call$ec2.path <- ec2.path.override
	}

	# Log user back into master node
	eval(parse(text=paste("system(\"cd ", call$ec2.path, "; ssh -i ", call$keypair, " root@", 
		 insts[[(node.num+1)]][[4]], " -o StrictHostKeyChecking=no\")", sep = "")))
} # END logOn function
