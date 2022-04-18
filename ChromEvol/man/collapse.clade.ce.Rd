\name{collapse.clade.ce}
\alias{collapse.clade.ce}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Collapses a clade in a ChromEvol tree
}
\description{
This function allows to collapse a clade of a phylogeny in a ChromEvol 
object, adjusting all node, branch and tip information. The name for the 
remaining tip can be given, information about thenumber of tips dropped 
will be given in parentheses. At the tip a pie with the chromosome number 
information of the most recent coommon ancestor of the clade will be 
plotted.}
\usage{
collapse.clade.ce(ChromEvol, node, name = "collapsed")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ChromEvol}{
Object of class ChromEvol
}
  \item{node}{
a vector of mode numeric indicating the number of the most recent common ancestor (MRCA) node of the clade to be collapsed.
}
  \item{name}{
a vector of mode character indicating name of the collpased clade to be shown as tip label in the tree 
}
  \item{numbers}{
a vector of mode characters indicating the chromosome numbers appearing in the collpased clade to be added to the tip label (e.g. "9 18")
}
}
\details{
The text to be plotted at the tips of the collapsed clade will have the following structure:
	"name (x tips)-numbers"
}
\value{
An object of class ChromEvol
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Natalie Cusimano
}
\note{
If more than one clade should be collapsed please start from top to bottom, otherwise the naming and the pies of the collapsed clades will be wrong
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{extract.clade.ce}}
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
