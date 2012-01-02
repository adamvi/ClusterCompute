######################################################################################################################################################
###  getFiles - Secure Copy (scp) results from the Master Node (node.number=1, or other) back to user's local machine.  
###	getFiles("data.Rdata")
###	User either has to log out of Master node to execute or execute from alternate (local) terminal instance of R

`getFiles` <-
function(files, node.number=0, local.directory, instance.directory="root/", ec2.path.override) {
	# Get the clusterConfig call to use arguments
	clst.call <- NULL
	try(clst.call <- as.call(unserialize(charToRaw(readChar("CLUSTER_CONFIG_FILES/call.txt", file.info("CLUSTER_CONFIG_FILES/call.txt")$size)))), silent = FALSE)
	if(is.null(clst.call)) {
		stop("The working directory must be same file as the one used to start and configure the cluster instance(s) - i.e. the parent file of CLUSTER_CONFIG_FILES.\n\n")
	}
	
	if (length(node.number) > 1) {
		node.number <- node.number[1]
		message("Only one node number can be specified at a time for getFiles().  The first node (", node.number,") will be used")
	}

	# Get instance id's from running instances (from file or by using ec2 Tools)
	options(warn=-1)
	insts<-NULL
	try(insts<-unserialize(charToRaw(readChar("CLUSTER_CONFIG_FILES/runningInstances.txt", 
		file.info("CLUSTER_CONFIG_FILES/runningInstances.txt")$size))), silent = TRUE)
	
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

	if (node.number > length(insts)-1) stop("The indicated node number does not exist. The 'node.number' argument must be less than the total number of nodes.")

	# Create directory to house the results OR check to make sure the local.directory exists
	if (!missing(local.directory)) {
		if (is.na(file.info(local.directory)$isdir)) {
			dir.create('RESULTS')
			local.directory <- paste(getwd(),'/RESULTS/', sep="")
		}
		message(paste("File directory ", local.directory, 
			" not found or incorrectly specified.  Files will be downloaded to a directory named 'RESULTS' created in current working directory.\n"), sep="")
		if (substring(local.directory, nchar(local.directory), nchar(local.directory))!="/") local.directory <- paste(local.directory, "/", sep="")
		if (substring(local.directory, 1, 1)!="/") local.directory <- paste("/", local.directory, sep="")
	}	else {
		if (is.na(file.info('RESULTS')$isdir)) {
			dir.create('RESULTS/')
		}
		local.directory <- paste(getwd(),'/RESULTS/', sep="")
		message("\nFiles will be downloaded to a new directory named 'RESULTS' created in current working directory.\n")
	}

	# Check the instance.directory
	if (substring(instance.directory, nchar(instance.directory), nchar(instance.directory))!="/") instance.directory <- paste(instance.directory, "/", sep="")
	if (substring(instance.directory, 1, 1)=="/") instance.directory <- substring(instance.directory, 2, nchar(instance.directory))
	if (substring(instance.directory, 1, 2)==":/") instance.directory <- substring(instance.directory, 3, nchar(instance.directory))

	options(warn=0)

	# Get files from node
	for (f in 1:length(files)) {
		eval(parse(text=paste("system('scp -r -P 22 -i ", clst.call$ec2.path, clst.call$keypair, " root@", insts[[(node.number+1)]][[4]], 
			":/", instance.directory, files[f], " ", local.directory, "')", sep = "")))
	}
} # END getFiles function
