\name{get.node.info}
\alias{get.node.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives information of specified node
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.node.info(path, nodes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{nodes}{
%%     ~~Describe \code{nodes} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (path, nodes) 
{
    cE <- read.cE(path)
    pp <- vector("list", 3)
    for (i in nodes) {
        pp.tree <- read.tree(paste(path, "/posteriorAncestors.tree", 
            sep = ""))
        pp.tree$node.label[i] <- gsub(paste("[[]N", as.character(i), 
            "_", sep = ""), "", pp.tree$node.label[i])
        pp.tree$node.label[i] <- gsub("[]]", "", pp.tree$node.label[i])
        pp[[1]] <- c(pp[[1]], gsub("//.*", "", pp.tree$node.label[i]))
        pp[[2]] <- c(pp[[2]], gsub(".*//", "", pp.tree$node.label[i]))
        pp[[3]] <- as.numeric(cE[[3]]$node.label[i])
    }
    pp <- as.data.frame(pp)
    dimnames(pp)[[1]] <- as.character(nodes)
    dimnames(pp)[[2]] <- c("Best_CN_PP", "2nd_best_CN_PP", "CN_ML")
    pp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
