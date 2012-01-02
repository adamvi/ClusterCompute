\name{killCluster}
\alias{killCluster}
\title{Terminate ALL cluster compute instances.}
\description{
Terminate running instances of Master and Worker nodes. User either has to log out of Master node to execute or execute from an alternate (local) instance of \code{R} with the same working directory.
}
\usage{
killCluster( trash=FALSE )
}

\arguments{
 \item{trash}{Logical.  Default is \code{FALSE}. If set to \code{TRUE} the *.Rout log files and the CLUSTER_CONFIG_FILES directory containing the configuration command files are PERMANENTLY deleted from the user's working directory.
  }
} 


\details{The \code{killCluster} function provides an easy way terminate the cluster. The \code{trash} argument cleans up the user's working directory by deleting files use for configuration of the cluster.  The log files (*.Rout) and the RESULTS file directory (if \code{getFiles} has been used) will remain in the working directory.
}

\author{Adam R. Van Iwaarden \email{vaniwaarden@colorado.edu} }

\seealso{\code{\link{clusterConfig}}, \code{\link{logOn}}, \code{\link{getFiles}}, \code{\link{sendFiles}}, \code{\link{killWorker}}}


\examples{
\dontrun{
## terminate cluster and remove all log and command files from the working Directory.

     killCluster(trash=TRUE)
}
}

\keyword{programming}
\keyword{interface}