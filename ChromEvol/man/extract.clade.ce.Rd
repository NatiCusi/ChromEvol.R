\name{extract.clade.ce}
\alias{extract.clade.ce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts a clade from a ChromEvol tree}
\description{
Extracts a clade from a phylogeny in a ChromEvol object, together with all node, branch and tip information
}
\usage{
extract.clade.ce(ce, node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ce}{
object of class ChromEvol
}
  \item{node}{
a vector of mode numeric number of the most recent common ancestor (MRCA) node of the clade to be extracted.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
object of class ChromEvol}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Natalie Cusimano
}
\note{
 In the events table columns "OnlySimBigger05" and "OnlyHeur" are not adapted to the extracted clade and should therefore not be considered (information still from the original, complete phylogeny).
 ONLY all events and all events >0.5 should be considered.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{collapse.clade.ce}}
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
