\name{get.PP}
\alias{get.PP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.PP(..., node = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{node}{
%%     ~~Describe \code{node} here~~
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
function (..., node = 1) 
{
    models <- list(...)
    models <- unlist(models)
    pp <- vector("list", 2)
    for (i in 1:length(models)) {
        pp.tree <- read.tree(paste(models[[i]], "/posteriorAncestors.tree", 
            sep = ""))
        pp.tree$node.label[node] <- gsub(paste("[[]N", node, 
            "_", sep = ""), "", pp.tree$node.label[node])
        pp.tree$node.label[node] <- gsub("[]]", "", pp.tree$node.label[node])
        pp[[1]] <- c(pp[[1]], gsub("//.*", "", pp.tree$node.label[node]))
        pp[[2]] <- c(pp[[2]], gsub(".*//", "", pp.tree$node.label[node]))
    }
    names(pp) <- c("CN_PP_root_BEST", "CN_PP_root_2ndB")
    pp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
