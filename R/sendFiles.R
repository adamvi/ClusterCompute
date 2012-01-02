######################################################################################################################################################
### sendFiles - Secure copy additional files from user's local machine to the (default = master) node(s)
###	
###	User either has to log out of Master node to execute or execute from alternate (local) terminal instance of R

`sendFiles` <-
function(new.files, node.numbers=0, local.directory, instance.directory="root/", ec2.path.override) {
	# Get the clusterConfig call to use arguments
	clst.call <- NULL
	try(clst.call <- as.call(unserialize(charToRaw(readChar("CLUSTER_CONFIG_FILES/call.txt", file.info("CLUSTER_CONFIG_FILES/call.txt")$size)))), silent = FALSE)
	if(is.null(clst.call)) {
		stop("The working directory must be the one used to start and configure the cluster instance(s) - i.e. the parent directory of CLUSTER_CONFIG_FILES and call.txt must be available.\n\n")
	}
	if (missing(local.directory)) local.directory <- getwd()	else {
		if (substring(local.directory, nchar(local.directory), nchar(local.directory))!="/") local.directory <- paste(local.directory, "/", sep="")
		if (substring(local.directory, 1, 1)!="/") local.directory <- paste("/", local.directory, sep="")
	}

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
	
	if (node.numbers > length(insts)-1) stop("The 'node.numbers' argument must be less than the total number of nodes.  Acceptable node numbers range from 0 (MASTER node) to 1 minus the number of RUNNING instances.")
	
	# Check the instance.directory
	if (substring(instance.directory, nchar(instance.directory), nchar(instance.directory))!="/") instance.directory <- paste(instance.directory, "/", sep="")
	if (substring(instance.directory, 1, 1)=="/") instance.directory <- substring(instance.directory, 2, nchar(instance.directory))
	if (substring(instance.directory, 1, 2)==":/") instance.directory <- substring(instance.directory, 3, nchar(instance.directory))

	if(!missing(ec2.path.override)) {
		clst.call$ec2.path <- ec2.path.override
		if (substring(clst.call$ec2.path,nchar(clst.call$ec2.path),nchar(clst.call$ec2.path))!="/") clst.call$ec2.path<-paste(clst.call$ec2.path,"/",sep="")
		if (substring(clst.call$ec2.path, 1, 1)!="/") clst.call$ec2.path <- paste("/", clst.call$ec2.path, sep="")
	}

	# Send files to node(s)
	for (n in node.numbers){
		for (f in 1:length(new.files)) {
			eval(parse(text=paste("system('scp -r -P 22 -o StrictHostKeyChecking=no -i ", clst.call$ec2.path, clst.call$keypair, " ", 
				local.directory, new.files[f], " root@", insts[[(n+1)]][[4]], ":/", instance.directory, ")", sep="")))
		}
	}
} # END sendFiles function
