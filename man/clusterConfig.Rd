\name{clusterConfig}
\alias{clusterConfig}
\title{Configure Compute Cluster}
\description{
Function provides seamless configuration of and access to a multi-node cluster of Amazon Web Services (AWS) Cluster Compute instances. Cluster compute nodes are configured to allow 
passwordless ssh communication between nodes, allows the user to execute linux system configuration commands on all compute nodes and update or install necessary 
R packages on all nodes. Files located in the user's working directory of \code{R} can be copied to either the master node or all nodes.
}
\usage{
clusterConfig(ami.id,		
              num.nodes = 1,
              slots=list(MASTER=c(6,6), WORKERS=c(8,8)),
              ec2.path,
              keypair,
              inst.type = "cc1.4xlarge",
              s.group,
              p.group,
              rerun = FALSE,
              packages,
              options = "dep = T",
              repos = "http://software.rc.fas.harvard.edu/mirrors/R/",
              master.files,
              node.files,
              xtra.Rcmd,
              xtra.SYScmd)
}

\arguments{
  \item{ami.id}{REQUIRED. Character string.  Amazon Machine Image (AMI) ID the user would like to launch instances of.  This Image must include, at a minimum, working copies of \code{R} and OpenMPI (or another message passing interface software) software.  An appropriate AMI must be identified or set up by the user before the function will work properly and before any useful cluster computation can occur (where OpenMPI is needed). 
  }
  \item{num.nodes}{Integer. The Number of (Cluster) Compute Nodes to initiate.  Default is 1, which only launches a single cluster compute instance.
  }
  \item{slots}{List. The Number of slots and max.slots to use in the machinefile.  Must be length = 2.  Default is to specify 6 slots and 6 max-slots for the master node and 8 slots and 8 max-slots for the worker node(s).  NOTE:  choice should depend on the cores/hyperthreads available on each instance type, as well as the demands on system memory that the analyses will demand.  The machinefile can be altered from the instance before running the analyses.  Default may be appropriate for a moderately memory intensive analysis using the cc1.4xlarge or cc2.8xlarge instance types.
  }
  \item{ec2.path}{REQUIRED. Character string.  File path to the user`s EC2 command line tools directory.
  }
  \item{keypair}{REQUIRED. Character string.  The name of the EC2 keypair (a .pem file) created through AWS prior to use. 
  }
  \item{inst.type}{Character string.  EC2 instance type.  Default is Cluster Compute, and unlikely to work with other instance types. 
  }
  \item{s.group}{REQUIRED. Character string.  Name of the security group the user has established (either in the AWS console or with the command line tools) prior to function use.
  }
  \item{p.group}{REQUIRED. Character string.  Name of the cluster placement group the user has established prior to function use with the command line tools.
  }
  \item{rerun}{Logical.  If set to \code{TRUE} the function will run without starting new instances.  This is necessary if the instances have not fully initialized before the system commands are run or an error has been made in the specification of the call to \code{clusterConfig}. In some cases the files will not be copied over to the Master node, or the cluster may not be set up properly.  Currently the function is set to wait for 4 minutes (120 seconds) after the instances are "running" before attempting to contact them.
  }
  \item{packages}{Character string. \code{R} packages the user needs installed on all nodes.  The AMI may already have many of the packages needed installed in the \code{R} library, but the user may need new packages installed or existing packages updated.  
  }
  \item{options}{Character string.  The \code{options} argument passed to \code{install.packages()}.  Default is to include package dependencies.
  }
 \item{repos}{Character string.  User`s preferred repository.  Default is Harvard since AWS cluster compute instances are currently located on the East Coast.
  } 
  \item{master.files}{Character string. Data files, \code{R} script files, etc. to upload to the instance via secure copy (scp) to the Master node.
  }
  \item{node.files}{Character string. Data files, \code{R} script files, etc. to upload to the instance via secure copy (scp) to ALL nodes.
  }
  \item{xtra.Rcmd}{Character string.   Additional remote command(s) (e.g. more complicated package installation) to execute on each cluster node`s copy of \code{R} prior to use.
  } 
  \item{xtra.SYScmd}{Character string.   Additional remote command(s) to execute on each cluster node`s Linux operating system as ROOT (e.g. R CMD INSTALL NewPackage_0.0.tar.gz)
  }
} 


\details{The \code{clusterConfig} function is meant to provide seamless configuration of and access to a multi-node cluster of AWS Cluster Compute instances through the specification of a single command on the user`s local copy of R.  (NOTE: R GUIs can not be used.  On Mac OS R must be started from the 'Terminal' app - located in Applications >> Utilities >> Terminal.  Start R by entering 'R' at the prompt.    Multiple sessions of R can be started in this way and used to manage the AWS Compute Cluster.)  A call to \code{clusterConfig} will create a subdirectory of command files in the user`s working directory of R.  Those files will be copied over to the cluster nodes and executed remotely to establish passwordless ssh communication between the nodes.  As the command files are executed from the user's local machine *.Rout log files are also produced and can provide vital diagnostic and debugging information.  Upon completion of file uploads and remote system command executions (the progress of which can be inspected in the *.Rout files), the user is automatically logged into the Master node as ROOT via ssh.
}

\author{Adam R. Van Iwaarden \email{vaniwaarden@colorado.edu} }

\seealso{\code{\link{logOn}}, \code{\link{sendFiles}}, \code{\link{getFiles}}, \code{\link{killWorker}},\code{\link{killCluster}}}

\examples{
\dontrun{

##  Start up a 3 node cluster using AWS machine image ami-762cc71f with
##  the user's credentials.  Install the data.table and plyr packages, upload 
##  the scripts "test_scripts.R" and "file.R"and the (fictional) custom package 
##  NewPackage.  Install NewPackage using 'R CMD INSTALL'

     setwd('/Path/to/Example_Working_Directory')
     
     clusterConfig(ami.id = "ami-762cc71f", 
                   num.nodes = 3, 
                   ec2.path = "/Path/to/EC2_Tools/",
                   keypair = "Cluster_Keypair.pem", 
                   s.group = "my.cluster.securegroup", 
                   p.group = "my.cluster.placegroup",
                   packages = c("data.table", "plyr"),
                   master.files = c("test_scripts.R", "file.R"),
                   node.files = "NewPackage_0.0.tar.gz",
                   xtra.SYScmd = " R CMD INSTALL NewPackage_0.0.tar.gz")
}
}

\keyword{programming}
\keyword{interface}