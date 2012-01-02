\name{killWorker}
\alias{killWorker}
\title{Terminate one or more cluster compute instances without terminating the entire cluster.}
\description{
Terminate running instances of any number of WORKER nodes. User either has to log out of Master node to execute or execute from an alternate (local) instance of \code{R} with the same working directory.
}
\usage{
killWorker( workers=length(insts)-1, 
         specific.node, 
         kill.master=FALSE )
}

\arguments{
  \item{workers}{Integer. The number of node(s) to terminate.  Default is to kill all workers, but a partial kill can be specified with an argument 'workers = n'.
  }
  \item{specific.node}{Integer. Optional - if provided, the specific node number that the user would like to terminate.
  }
  \item{kill.master}{Logical.  Default is \code{FALSE}. If set to \code{TRUE} the user may select the master node as the specific node to kill (\code{specific.node=0}).
  }
} 


\details{The \code{killWorker} function provides an easy way terminate any number of nodes in the cluster.  If the user attempts to terminate all nodes or the final remaining (Master) node, they will be prompted to confirm termination.  The default behavior is to terminate ALL worker nodes, however a partial kill can be requested by setting workers to the desired number of nodes to be terminated.  A specific node can also be terminated (which may be desirable if, for example, a specific node is not functioning properly).  The \code{kill.master} argument is an additional safeguard against accidental requests to kill the Master node.
}

\author{Adam R. Van Iwaarden \email{vaniwaarden@colorado.edu} }

\seealso{\code{\link{clusterConfig}}, \code{\link{logOn}}, \code{\link{getFiles}}, \code{\link{sendFiles}}, \code{\link{killCluster}}}


\examples{
\dontrun{
## terminate 3 worker nodes of a cluster (assuming there are more than 3 worker nodes)

     killWorker(workers=3)

## terminate the third node of a cluster

     killWorker(specific.node=2)
}
}

\keyword{programming}
\keyword{interface}