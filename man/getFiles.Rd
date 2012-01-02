\name{getFiles}
\alias{getFiles}
\title{Get one or more files from a single cluster compute node}
\description{
Download files from one of the cluster nodes to user's machine. User either has to log out of Master node to execute or execute from an alternate (local) instance of \code{R} with the same working directory.
}
\usage{
getFiles( files,
         node.number=0,
         local.directory, 
         instance.directory='root/',
         ec2.path.override )
}

\arguments{
  \item{files}{Character string. Files to download from the instance via secure copy (scp) to the user's local machine.
  }
  \item{node.number}{Integer. The node number from which to download the file. Default is 0, which downloads files from the MASTER node.
  }
  \item{local.directory}{Character string. Name of file directory to save the downloaded files. If missing, a file named "RESULTS" will be created within the working directory and files with be downloaded to that file.
  }
  \item{instance.directory}{Character string. Full path for the directory on the remote instance where the files will be uploaded to.  Default is the root directory
  }
  \item{ec2.path.override}{Character. Alternative path to EC2 API tools used to log onto Compute Nodes.  Can be used if more than one user wishes to log onto a node from different local machines.  Assumes both users have a copy of the CLUSTER_CONFIG_FILES directory (particularly the call.txt file, as well as the pk-***.pem cert-***.pem certifications, and the keypair.pem file used to start up the cluster in their respective EC2 Tools directories).
  }
} 


\details{The \code{getFiles} function provides an easy way download files or entire directories from any one node in the cluster.  Unless otherwise specified, a new file directory named "RESULTS" is created within the working directory to house the downloaded file(s) and directories.  Function issues a system call to  \code{scp} using port 22 (NOTE: this port may need to be opened in the AWS Security Group).  An example of the internal call made would be 'scp  -r  -P 22  -i /Path/to/EC2_Tools/Cluster_Keypair.pem  root@ec2-XXX.amazonaws.com:/root/Example.file  /Path/to/Example_Working_Directory'
}

\author{Adam R. Van Iwaarden \email{vaniwaarden@colorado.edu} }

\seealso{\code{\link{clusterConfig}}, \code{\link{logOn}}, \code{\link{sendFiles}}, \code{\link{killWorker}}, \code{\link{killCluster}}}


\examples{
\dontrun{
##  get file from the master node - download to (and create if not there) 
##  the 'RESULTS' file in the working directory

     getFiles(files="file_name.extension")

##  Download analysis results file named "results.Rdata" and R log file "script.Rout" 
##  to the "Alternative_Directory" not located in the user's working directory.

     getFiles(files=c("results.Rdata", "script.Rout" ), 
           local.directory="/Path/to/Alternative_Directory")
}
}

\keyword{programming}
\keyword{interface}