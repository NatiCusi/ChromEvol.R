\name{sum.Parameters}
\alias{sum.Parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizes inferred parameters
}
\description{
summarizes the parameters of one model stored in an object of class ChromEvol.
}
\usage{
sum.Parameters(ChromEvol, node = 1, write.table = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ChromEvol}{
object of class ChromEvol
}
  \item{node}{
specifies a node of interest for which the inferred chromosome numbers will be 
summarized, default is 1, i.e. the root node.

}
  \item{write.table}{
if TRUE the summary is written in a file called "sumParameters"
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


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
