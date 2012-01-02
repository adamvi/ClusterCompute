\name{logOn}
\alias{logOn}
\title{Node Log On Via ssh}
\description{
Log on to the Master or Worker Node from user's local machine. User either has to log out of Master node to execute or execute from an alternate (local) instance of \code{R} with the same working directory.
}
\usage{
logOn( node.num = 0,
       ec2.path.override )
}

\arguments{
  \item{node.num}{Integer. The Number of (Cluster) Compute Nodes to log onto.  Default is 0, which logs the user into the MASTER cluster compute instance. Max value is then 1 minus the number of nodes in the cluster.
  }
  \item{ec2.path.override}{Character. Alternative path to EC2 API tools used to log onto Compute Nodes.  Can be used if more than one user wishes to log onto a node from different local machines.  Assumes both users have a copy of the CLUSTER_CONFIG_FILES directory (particularly the call.txt file, as well as the pk-***.pem cert-***.pem certifications, and the keypair.pem file used to start up the cluster in their respective EC2 Tools directories).
  }
}


\details{The \code{logOn} function is meant to provide an easy way to log into ANY node in the cluster as ROOT.  The function issues a system command to \code{ssh} to gain remote access through the terminal in which R is running locally (NOTE: R GUIs can not be used on Mac OS - R must be started from the 'Terminal' app - located in Applications >> Utilities >> Terminal.  Start R by entering 'R' at the prompt.  Multiple sessions of R can be started in this way and used to manage the AWS Compute Cluster.).
}

\author{Adam R. Van Iwaarden \email{vaniwaarden@colorado.edu} }

\seealso{\code{\link{clusterConfig}}, \code{\link{sendFiles}}, \code{\link{getFiles}}, \code{\link{killWorker}},\code{\link{killCluster}}}


\examples{
\dontrun{
## log on to master node

     logOn()

## log on to the last worker node in a 4 node cluster

     logOn(node.num=3)
}
}

\keyword{programming}
\keyword{interface}