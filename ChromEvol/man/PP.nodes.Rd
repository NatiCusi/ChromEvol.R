\name{PP.nodes}
\alias{PP.nodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives posterior porbability for inferred numbers at specified nodes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PP.nodes(ChromEvol, nodes, cutoff = 0.1, percentage = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ChromEvol}{
object of class ChromEvol
}
  \item{nodes}{
numbers of nodes of interest, numbers can be according to chromEvol or phylo node numbering
}
  \item{cutoff}{
only chromosome numbers with a posterior probability >cut.off will be reported. cuto.off wll be adjusted if the sum of the resulting PPs are smaller than \code{percentage}
}
  \item{percentage}{
the sum of the reported numbers will be >= \code{percentage} here~~
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

## }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
