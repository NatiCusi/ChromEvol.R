\name{write.param}
\alias{write.param}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.param(filename, mainType, maxChrNum, minChrNum, branchMul, rootAt, numberSim = numberSim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{mainType}{
%%     ~~Describe \code{mainType} here~~
}
  \item{maxChrNum}{
%%     ~~Describe \code{maxChrNum} here~~
}
  \item{minChrNum}{
%%     ~~Describe \code{minChrNum} here~~
}
  \item{branchMul}{
%%     ~~Describe \code{branchMul} here~~
}
  \item{rootAt}{
%%     ~~Describe \code{rootAt} here~~
}
  \item{numberSim}{
%%     ~~Describe \code{numberSim} here~~
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
function (filename, mainType, maxChrNum, minChrNum, branchMul, 
    rootAt, numberSim = numberSim) 
{
    wd <- getwd()
    if (mainType == "All_Models") 
        write(paste("_mainType", mainType), file = paste(filename, 
            "_params.txt", sep = ""))
    else {
        if (length(grep("fix_", mainType)) != 1) 
            write("_mainType Optimize_Model", file = paste(filename, 
                "_params.txt", sep = ""))
        else write("_mainType Run_Fix_Param", file = paste(filename, 
            "_params.txt", sep = ""))
    }
    write(paste("_outDir", wd), file = paste(filename, "_params.txt", 
        sep = ""), append = TRUE)
    write(paste("_dataFile ", wd, "/", filename, "_input_table.txt", 
        sep = ""), file = paste(filename, "_params.txt", sep = ""), 
        append = TRUE)
    write(paste("_treeFile ", wd, "/", filename, "_input.tree", 
        sep = ""), file = paste(filename, "_params.txt", sep = ""), 
        append = TRUE)
    write(paste("_logFile ", filename, "_log1.txt", sep = ""), 
        file = paste(filename, "_params.txt", sep = ""), append = TRUE)
    write(paste("_maxChrNum", maxChrNum), file = paste(filename, 
        "_params.txt", sep = ""), append = TRUE)
    write(paste("_minChrNum", minChrNum), file = paste(filename, 
        "_params.txt", sep = ""), append = TRUE)
    write(paste("_rootAt", rootAt), file = paste(filename, "_params.txt", 
        sep = ""), append = TRUE)
    write(paste("_branchMul", branchMul), file = paste(filename, 
        "_params.txt", sep = ""), append = TRUE)
    write(paste("_simulationsNum", numberSim), file = paste(filename, 
        "_params.txt", sep = ""), append = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
