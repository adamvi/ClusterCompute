######################################################################################################################################################
### killWorker - terminates worker node instances.  
###	Default is to kill all workers, but a partial kill can be specified with an argument 'workers = n'
###	User either has to log out of Master node to execute or start another instance of R, change working directory, etc. and execute

`killWorker` <-
function(workers=length(insts)-1, specific.node, kill.master=FALSE) {

	# Get the clusterConfig call to use arguments
	clst.call <- NULL
	try(clst.call <- as.call(unserialize(charToRaw(readChar("CLUSTER_CONFIG_FILES/call.txt", file.info("CLUSTER_CONFIG_FILES/call.txt")$size)))), silent = FALSE)
	if(is.null(clst.call)) {
		stop("The working directory must be same file as the one used to start and configure the cluster instance(s) - i.e. the parent file of CLUSTER_CONFIG_FILES.\n\n")
	}

	if (specific.node==0 & !kill.master) stop("You have indicated termination of the MASTER node (node number 0).  
		The 'kill.master' argument must be set to 'TRUE' to terminate MASTER node and .")

	if (specific.node > 0 & kill.master) stop("The 'kill.master' argument has been set to 'TRUE' but the MASTER node (node number 0) has not been selected.  
		The arguments must be set to 'specific.node = 0' and 'kill.master = TRUE' to terminate MASTER node.")
		
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

	if (length(insts)==1) terminate <- FALSE
	while (!terminate) {
		kill <- readline(prompt = "\n\nOnly one (MASTER) node instance is currently running!  Do you want to terminate the MASTER NODE instance?  [y/n]")
		if (toupper(kill)=="N") stop("\n\nkillWorker() aborted.  Master node instance is still running.")
		if (toupper(kill)=="Y") terminate <- TRUE else next
	}

	if (workers == length(insts)) terminate <- FALSE
	while (!terminate) {
		kill <- readline(prompt = "\n\nYou have requested more WORKERS than are currently running.  This will terminate the entire CLUSTER.  Do you want to continue?  [y/n]")
		if (toupper(kill)=="N") stop("\n\nkillWorker() aborted.  All instances are still running.")
		if (toupper(kill)=="Y") terminate <- TRUE else next
	}

	if (!missing(specific.node)) {
		eval(parse(text=paste("system(\"ec2-terminate-instances ", insts[[specific.node+1]][[2]], "\")", sep = "")))
	} else {
		for (b in length(insts):(length(insts)-workers+1)) eval(parse(text=paste("system(\"ec2-terminate-instances ", insts[[b]][[2]], "\")", sep = "")))
	}
	#delete runningInstances file so that if more workers are killed in future, the fuction identifies a running node.
	unlink("CLUSTER_CONFIG_FILES/runningInstances.txt")

	# Log user back into master node
	eval(parse(text=paste("system(\"cd ", clst.call$ec2.path, "; ssh -i ", clst.call$keypair, " root@", 
		insts[[1]][[4]], " -o StrictHostKeyChecking=no\")", sep = "")))
} # END killWorker function
