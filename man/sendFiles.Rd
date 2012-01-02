\name{sendFiles}
\alias{sendFiles}
\title{Send one or more files to any number of cluster compute nodes}
\description{
Send files from user's machine to any number of the cluster nodes. User either has to log out of Master node to execute or execute from an alternate (local) instance of \code{R} with the same working directory.
}
\usage{
sendFiles( new.files, 
           node.numbers=0,
           local.directory,
           instance.directory='root/',
           ec2.path.override)
}

\arguments{
  \item{new.files}{Character string. Data files, \code{R} script files, etc. to upload to the instance via secure copy (scp) to the Master node.
  }
  \item{node.numbers}{Integer. The number of node(s) to send files to.  Default is 0, which sends files to the MASTER cluster compute instance. Providing more than one value (any integer from zero to 1 minus the number of nodes in the cluster) will send the file to each of those nodes.
  }
  \item{local.directory}{Character string. Full path for the directory on the local machine where the files to be uploaded are located. If missing the working directory will be assumed.
  }
  \item{instance.directory}{Character string. Full path for the directory on the remote instance where the files will be uploaded to.  Default is the root directory
  }
  \item{ec2.path.override}{Character. Alternative path to EC2 API tools used to log onto Compute Nodes.  Can be used if more than one user wishes to log onto a node from different local machines.  Assumes both users have a copy of the CLUSTER_CONFIG_FILES directory (particularly the call.txt file, as well as the pk-***.pem cert-***.pem certifications, and the keypair.pem file used to start up the cluster in their respective EC2 Tools directories).
  }
} 


\details{The \code{sendFiles} function provides an easy way send files to ANY of the nodes in the cluster.  Function issues a system call to \code{ssh} using port 22 (NOTE: this port may need to be opened in the AWS Security Group).  An example of the internal call made would be 'scp  -r  -P 22  -i /Path/to/EC2_Tools/Cluster_Keypair.pem  /Path/to/Example_Working_Directory/Example.file  root@ec2-XXX.amazonaws.com:/root/'.
}

\author{Adam R. Van Iwaarden \email{vaniwaarden@colorado.edu} }

\seealso{\code{\link{clusterConfig}}, \code{\link{logOn}}, \code{\link{getFiles}}, \code{\link{killWorker}}, \code{\link{killCluster}}}


\examples{
\dontrun{
##  All examples assume files are located in the R working directory.
## send file to master node

     sendFiles(new.files="file_name.extension")

## send files named "data.csv" and "script.R" to (the first) 3 nodes

     sendFiles(new.files=c("data.csv", "script.R"), node.numbers=0:2)
}
}

\keyword{programming}
\keyword{interface}