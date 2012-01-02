######################################################################################################################################################
####
####	clusterConfig function starts up and configures EC2 Cluster Compute Nodes
####	- allow passwordless ssh between nodes
####	- send machine file to the master node
####	- send data and other files to the nodes
####	- install (or update) R packages on all compute nodes
####	- Execute Linux system commands on all compute nodes
####
######################################################################################################################################################

### Example function call:
###
#	clusterConfig(ami.id = "ami-762cc71f", 
#				 num.nodes = 2, 
#				 ec2.path = "/Path/to/.ec2/",
#				 keypair = "Cluster_Keypair.pem", 
#				 s.group = "my.cluster.securegroup", 
#				 p.group = "my.cluster.placegroup",
#				 packages = c("SGP", "plyr"),
#				 master.files = c("test_scripts.R", "file.R"),
#				 node.files = "data.table_1.5.1.tar.gz",
#				 xtra.SYScmd = c("dd if=/dev/zero of=/mnt/swap bs=1M count=5000", "mkswap /mnt/swap",
#								 "swapon /mnt/swap", "sysctl -w vm.swappiness=80"))
###
###  End example call  ###

`clusterConfig` <- 
	function(ami.id,			#	Amazon Machine Image ID
	num.nodes = 1,				#	Number of (Cluster) Compute Nodes to initiate
	slots=list(MASTER=c(6,6), WORKERS=c(8,8)),	#	Number of slots and max.slots to use in the machinefile.  Must be length = 2 
	ec2.path,					#	The file path to the users EC2 Tools directory (where tools and .pem files are kept)
	keypair,					#	EC2 keypair name - a .pem file
	inst.type = "cc1.4xlarge",  #	EC2 Instance type - not sure if other (non-HPC) instance types can be used...
	s.group,					#	Name of the security group (assumed to be set up prior to use)
	p.group,					#	Name of the placement group (assumed to be set up prior to use)
	rerun = FALSE,				#	The function can be rerun without starting new instances with rerun = TRUE
	packages,					#	R packages needed to be installed on all nodes. Passed as a concatenated string (e.g. c("plyr", "SGP"))
	options = "dep = T",		#	Options argument passed to EACH package installation as ONE string.  Default includes dependencies.
	repos = "http://software.rc.fas.harvard.edu/mirrors/R/", #	User's prefered repository.  Default is Harvard - AWS cluster compute instances are in East Coast region.
	master.files,				#	Data files, R script files, etc. user wants to secure copy (scp) to the Master node.
	node.files,					#	Data files, R script files, etc. user wants to secure copy (scp) to the ALL nodes.
	xtra.Rcmd,					#	Additional (or more complicated) commands to execute remotely on each instance of R.
	xtra.SYScmd) {				#	Commands to execute remotely on each node as ROOT (e.g. R CMD INSTALL NewPackage_0.0.tar.gz)

	### Check to make sure working directory file name is valid (no spaces) and ec2.path ends in a forward slash (for Mac and Linux).
	wd <- getwd()
	if (length(grep(" ", wd))) stop(paste("Working directory file name can not contain spaces.\n\t	Please re-name file or change directory"))
	
	if (missing(ec2.path)) stop("ec2.path argument is missing.  The path to the EC2 API Tools is REQUIRED.")
	if (substring(ec2.path, nchar(ec2.path), nchar(ec2.path))!="/") ec2.path<- paste(ec2.path, "/", sep="")
	
	### Create directory to house the source file
	if (is.na(file.info("CLUSTER_CONFIG_FILES")$isdir)) {
		dir.create("CLUSTER_CONFIG_FILES")	}

	call <- match.call(expand.dots = FALSE)
	cat(rawToChar(serialize(call, NULL, ascii=T)),  "\n", sep = "", file = "CLUSTER_CONFIG_FILES/call.txt") 

	###  Start up instances using EC2 Tools command line application
	if (!rerun) {
		kp <- gsub(".pem", "", keypair)
		eval(parse(text=paste("system('cd ", ec2.path, "; ec2-run-instances ", ami.id, " -n ", num.nodes, " -g ",  s.group,
			" -k ",  kp," --instance-type ", inst.type, " --placement-group ", p.group,  "')", sep = "")))

	### Query EC2 Tools for running instances and get machine names 
	# New code based on JD Long: https://gist.github.com/478930#file_start_ec2_instance_ssh.r
	notRunning <- TRUE; runningInstances <- NULL
	while(notRunning) {
		while(length(runningInstances)!=num.nodes) {
			instancesParsed <- sapply(system("ec2-describe-instances", intern=TRUE), strsplit, "\t")
			runningInstances <- NULL; machinenames <- NULL; ri <- 1
				for (a in 1:length(instancesParsed)) {
					if (instancesParsed[[a]][[1]]=="INSTANCE") {
						if (instancesParsed[[a]][[6]]=="running") {
							runningInstances[[ri]] <- instancesParsed[[a]]
							machinenames[[ri]] <- runningInstances[[ri]][[4]]
							startTime <- strptime(runningInstances[[ri]][[11]], "%Y-%m-%dT%H:%M:%S")
							ri <- ri + 1
							notRunning <- FALSE
						} else notRunning <- TRUE
					} #END if (instancesParsed[[a]][[1]]=="INSTANCE")
				} # END a loop
			} # END while(length(...))
	} # END while(notRunning)
	
	#  Instances don't fully start up even when status is "running."  Wait another 2 minutes.  
	#  If still not running, rerun function with rerun = TRUE.
	Sys.sleep(120)

	if (num.nodes == 1) {
		cat("\n\n\nEC2 Session begun at ", as.character(startTime), 
			"\nSingle Cluster Compute Instance now running.  Files will be copied to it and remote commands executed.",
			"\nProgress and debugging info can be checked in the singleNode_#.Rout files in the working directory\n\n\n")
	}	else { 
		cat("\n\n\nEC2 Session begun at ", as.character(startTime), 
			"\nInstances now running.  Files will be copied to Master Node, passwordless SSH established between nodes and remote commands executed.",
			"\nProgress and debugging info can be checked in the Local_CMD.Rout files in the working directory\n\n\n") 
		}
	} # END if(!rerun) - Start up instances 

	if (rerun) {
		instancesParsed <- sapply(system("ec2-describe-instances", intern=TRUE), strsplit, "\t")
		runningInstances <- NULL; machinenames <- NULL; ri <- 1
		for (b in 1:length(instancesParsed)) {
			if (instancesParsed[[b]][[1]]=="INSTANCE") {
				if (instancesParsed[[b]][[6]]=="running") {
					runningInstances[[ri]] <- instancesParsed[[b]]
					machinenames[[ri]] <- instancesParsed[[b]][[4]]
					ri <- ri + 1
				}
			}
		}
	} # END if (rerun)

	# serialize and save 'runningInstances' object to use later in killCluster and killWorker functions (if executed from another instance of R)
	# Moved outside of notRunning loop so that it is re-written if "rerun."
	cat(rawToChar(serialize(runningInstances, NULL, ascii=TRUE)),  "\n", sep = "", file = "CLUSTER_CONFIG_FILES/runningInstances.txt")

	######################################################################################################################################################
	###
	###		Configure Single-node Instance
	###
	######################################################################################################################################################

	if (num.nodes == 1) {

		# Update or install new R packages on the Instance
		singleNode <- 0
		if (!missing(packages)) {
			cat("options(repos=c(CRAN='", repos, "'))\n", sep="", file="CLUSTER_CONFIG_FILES/remoteRstuff.R")
			for (w in 1:length(packages)) {
				cat("install.packages('", packages[w], "', ", options, ")\n", sep="", file="CLUSTER_CONFIG_FILES/remoteRstuff.R", append=TRUE)
			}
		singleNode <-1
		}
 
		if (!missing(xtra.Rcmd)) {
			if (singleNode == 0) apnd<-FALSE else apnd<-TRUE
			for (x in 1:length(xtra.Rcmd)) {
		  		cat(xtra.Rcmd[x], "\n", sep="", file="CLUSTER_CONFIG_FILES/remoteRstuff.R", append=apnd)
			}
			singleNode <-1
		}

		if (singleNode == 1) { # 1) copy the file 2) execute it remotely once its there.
			cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", 
				wd, "/CLUSTER_CONFIG_FILES/remoteRstuff.R root@", machinenames[1], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", 
				file="CLUSTER_CONFIG_FILES/singleNode_0.R")

			cat("while(system('cd ", ec2.path, "; ssh -i ", keypair, " root@", machinenames[1],
				" -o StrictHostKeyChecking=no 'R CMD BATCH remoteRstuff.R | exit'')!=0) {Sys.sleep(3)}\n", sep = "", 
				file="CLUSTER_CONFIG_FILES/singleNode_0.R", append=TRUE)
		}

		# scp files to the Instance
		count <- 1
		if (!missing(master.files)) for (y in singleNode:(singleNode + length(master.files))) {
			cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", wd, "/",
			master.files[count], " root@", machinenames[1], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", 
				file=paste("CLUSTER_CONFIG_FILES/singleNode_", y, ".R", sep=""))
			singleNode <- y; count <- count+1
		}

		# Try the command 2 times - if it doesn't work don't hold up the process.  User will need to execute the system command on each node.
		count <- 1
		if (!missing(xtra.SYScmd)) for (z in singleNode:(singleNode + length(xtra.SYScmd))) {
			cat("ss = 0\ntry(ss<-system('cd ", ec2.path, "; ssh -i ", keypair, " root@", machinenames[1],
				" -o StrictHostKeyChecking=no '", xtra.SYScmd[count], " | exit''), silent = TRUE)\n  if (ss!=0) {try(system('cd ", ec2.path, "; ssh -i ",
				keypair, " root@", machinenames[1], " -o StrictHostKeyChecking=no '", xtra.SYScmd[count], " | exit''), silent = TRUE)}\n\n", sep = "", 
				file=paste("CLUSTER_CONFIG_FILES/singleNode_", z, ".R", sep=""))
			singleNode <- z; count <- count+1
		}

		# Batch process the singleNode_#.R scripts on the users local machine:
		for (aa in singleNode:0){
			if (aa==0) batch<-".R')" else batch <- ".R &')"
			eval(parse(text=paste("system('R CMD BATCH CLUSTER_CONFIG_FILES/singleNode_", aa, batch, sep = "")))
		}

		# Log the user into the master node from local machine.
		eval(parse(text=paste("system('cd ", ec2.path, "; ssh -i ", keypair, " root@", machinenames[1], " -o StrictHostKeyChecking=no')", sep = "")))
		message("\n\t\tConnection to instance has been closed.  If instance is still running, connect again using 'logOn()'")
	} # END Single Instance Configuration

	######################################################################################################################################################
	###
	###		Configure Multiple-node Cluster
	###
	######################################################################################################################################################

	#  Create Local_CMD.R file - copy the keypair file, and copy node#_x.R files to each node.  Execute Node#_a.R file.  Sequential necessary
	#  'while' loops used througout to ensure that the operation is performed.  Several times I've noticed 'operation timed out' / 'lost connection'
	#  issues come up.  This is problematic enough that its best to just wait for the each expression to be executed.

	# Secure copy the keypair file over to the worker nodes
	for (d in 1:length(machinenames)) {
		if (d == 1) apnd<-FALSE else apnd<-TRUE
		cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", ec2.path, keypair," root@", 
			machinenames[d], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", file="CLUSTER_CONFIG_FILES/Local_CMD.R", append=apnd)
	}
	
	# Create the first set of node command files (one per node)
	# This set creates a dsa passwordless keypair for each node and concatenates it to its own authorized key file
	for (j in 1:length(machinenames)) {
		cat("system('mv /root/", keypair, " .ssh')\n",sep="", 
			file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_a.R", sep=""))
		cat("system('cd .ssh; ssh-keygen -t dsa -P '' -f ~/.ssh/id_dsa')\n", sep="", 
			file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_a.R", sep=""), append=TRUE)
		cat("system('cd .ssh; cat id_dsa.pub >> authorized_keys')\n", sep="", 
			file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_a.R", sep=""), append=TRUE)

	#  Copy the newly generated dsa keypair to the other nodes
		for (k in 1:length(machinenames)) {
			 if (k!=j) {
			 	cat("system('cd .ssh; scp -r -o StrictHostKeyChecking=no -i ", keypair, " id_dsa.pub root@", 
					machinenames[k], ":.ssh/authorized_keys", (j+1), "')\n", sep="", 
					file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_a.R", sep=""), append=TRUE)
			}
		}
	# Local_CMD file commands to secure copy the Node#_x.R files to the appropriate node.
		for (l in 1:length(machinenames)) {
			if (l==j) {
				cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", wd, 
					"/CLUSTER_CONFIG_FILES/Node", (j-1),"_a.R root@", machinenames[j], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", 
					file="CLUSTER_CONFIG_FILES/Local_CMD.R", append=TRUE)
				cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", wd, 
					"/CLUSTER_CONFIG_FILES/Node", (j-1),"_b.R root@", machinenames[j], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", 
					file="CLUSTER_CONFIG_FILES/Local_CMD.R", append=TRUE)
			}
		}
	# Create the second set of node command files (one per node)
	# Concatenate the other nodes' dsa keypair to its authorized key file
	for (m in 1:length(machinenames)) {
		if (m!=j) {
			if (m==1) apnd<-FALSE else apnd<-TRUE
			cat("system('cd .ssh; cat authorized_keys", (m+1), " >> authorized_keys')\n", 
				sep="", file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_b.R", sep=""), append=apnd)
		}
	}
	# Update or install new R packages on each node
		if (!missing(packages)) {
			cat("options(repos=c(CRAN='", repos, "'))\n", sep="", 
				file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_b.R", sep=""), append=TRUE)

			for (n in 1:length(packages)) {
				cat("install.packages('", packages[n], "', ", options, ")\n", sep="", 
					file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_b.R", sep=""), append=TRUE)
			}
		}
		
		if (! missing(xtra.Rcmd)) {
			for (o in 1:length(xtra.Rcmd)) {
				cat(xtra.Rcmd[o], "\n", sep="", file=paste("CLUSTER_CONFIG_FILES/Node", (j-1),"_b.R", sep=""), append=TRUE)
			}
		}
	} # END j loop - All Node Common Command File creation

	# Execute command files remotely (from user's local machine) once files have been copied over:
	# Log into each instance as ROOT, R CMD run the first command file (Node#_a.R):	  
	for (n in 1:length(machinenames)) {
		cat("while(system('cd ", ec2.path, "; ssh -i ", keypair, " root@", machinenames[n], 
			" -o StrictHostKeyChecking=no 'R CMD BATCH Node", (n-1),"_a.R | exit'')!=0) {Sys.sleep(3)}\n\n", sep = "", 
			file="CLUSTER_CONFIG_FILES/Local_CMD.R", append=TRUE)
	} # END Local_CMD.R file Creation

	# Create Node Specific Local_CMD_#.R files to be executed in BATCH form
	# Log into each instance as ROOT, R CMD run the second command file (Node#_b.R):	  
	for (p in 1:length(machinenames)) {
		cat("while(system('cd ", ec2.path, "; ssh -i ", keypair, " root@", machinenames[p],
			" -o StrictHostKeyChecking=no 'R CMD BATCH Node", (p-1),"_b.R | exit'')!=0) {Sys.sleep(3)}\n\n", sep = "", 
			file=paste("CLUSTER_CONFIG_FILES/Local_CMD_", (p-1), ".R", sep = ""))
	}

	#  Log into each instance as ROOT, run the Xtra SYSTEM commands file:	  
	for (q in 1:length(machinenames)) {
	# Create Machine file named 'machinefile'
		if (q==1) {
			apnd<-FALSE
			avail.slots <- slots[["MASTER"]][1]; max.slots <- slots[["MASTER"]][2]
		}	else {
			apnd<-TRUE
			avail.slots <- slots[["WORKERS"]][1]; max.slots <- slots[["WORKERS"]][2]
		}
		cat(machinenames[q], " slots = ", avail.slots, " max-slots = ", max.slots, "\n", sep="", file="CLUSTER_CONFIG_FILES/machinefile", append=apnd)
	# Secure copy the machinefile file and data files over to the Master Node
		if (q==1) {
			cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", wd, 
				"/CLUSTER_CONFIG_FILES/machinefile root@", machinenames[1], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", 
				file="CLUSTER_CONFIG_FILES/Local_CMD_0.R", append=TRUE)

			if (!missing(master.files)) for (f in 1:length(master.files)) {
				cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", wd, "/", 
					master.files[f], " root@", machinenames[q], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", 
					file="CLUSTER_CONFIG_FILES/Local_CMD_0.R", append=TRUE)
		 	}
		}
	# Secure copy the common files over to ALL Nodes (including Master)
		if (!missing(node.files)) for (r in 1:length(node.files)) {
			cat("while(system('cd ", ec2.path, "; scp -r -P 22 -o StrictHostKeyChecking=no -i ", keypair, " ", wd, "/", node.files[r],
				" root@", machinenames[q], ":/root')!=0) {Sys.sleep(3)}\n\n", sep="", 
				file=paste("CLUSTER_CONFIG_FILES/Local_CMD_", (q-1), ".R", sep = ""), append=TRUE)
		}
	# Try the command 2 times - if it doesn't work don't hold up the process.  User will need to execute the system command on each node.

		if (!missing(xtra.SYScmd)) for (s in 1:length(xtra.SYScmd)) {
			cat("ss = 0\ntry(ss<-system('cd ", ec2.path, "; ssh -i ", keypair, " root@", machinenames[q],
				" -o StrictHostKeyChecking=no '", xtra.SYScmd[s], " | exit''), silent = TRUE)\n  if (ss!=0) {try(system('cd ", ec2.path, "; ssh -i ",
				keypair, " root@", machinenames[q], " -o StrictHostKeyChecking=no '", xtra.SYScmd[s], " | exit''), silent = TRUE)}\n\n", sep = "", 
				file=paste("CLUSTER_CONFIG_FILES/Local_CMD_", (q-1), ".R", sep = ""), append=TRUE)
	 	}
	} # END q loop - Local_CMD_#.R creation

	# Run the Local_CMD.R and Local_CMD#.R scripts on the users local machine:
	system("R CMD BATCH CLUSTER_CONFIG_FILES/Local_CMD.R")
	for (t in length(machinenames):1){
		if (t==1) batch<-".R')" else batch <- ".R &')"
		eval(parse(text=paste("system('R CMD BATCH CLUSTER_CONFIG_FILES/Local_CMD_", (t-1), batch, sep = "")))
	}

	# Log the user into the master node from local machine.
	eval(parse(text=paste("system('cd ", ec2.path, "; ssh -i ", keypair, " root@", 
		machinenames[1], " -o StrictHostKeyChecking=no')", sep = "")))
	message("\n\t\tConnection to instance has been closed.  If instance is still running, connect again using 'logOn()'")
} # END clusterConfig function

