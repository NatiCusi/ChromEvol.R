\name{ce.input}
\alias{ce.input}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates the three required input files for the ancestral state reconstruction of chromosome numbers with chromEvol (Mayrose et al., 2010)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ce.input(phy, table, missing = NA, filename = "chromEvol_analysis", folder=NA, percentage = TRUE, 
mainType = "All_Models", fix.root=FALSE, maxChrNum = "-10", minChrNum = "1", branchMul = "1", 
rootAt = "N1", numberSim = NA, ladderize = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{
     The phylogeny on which the reconstruction should be made, either the path of a text file including the tree in newick format (readable with the ape function \code{read.tree}()) or an object of class phylo. Names in table and phylogeny must match.
}
  \item{table}{
Path of text file or R object with table consisting of two colums, 1st being species names, 2nd haploid chromosome numbers. The text file can also be already in FASTA format as required by ChromEvol. Multiple counts have to be separated with underline "_" (e.g. 12_24). Probabilities of chromosome numbers can also be given, they must sum up to 1, again numbers have to be spearated with "_" (e.g. 12=0.7_24=0.6). Missing number are indicated with NA.
Species names within table must match the tips of the tree. 
}
  \item{missing}{
if \code{missing} = NA (Default) all species for which no chromosome number is specified in the table are dropped from tree and table. When \code{missing} = "X", X is assigend to all species with missing chromosome number in the table and analysis is run with all species.
}

  \item{filename}{
optional. Defines a name for the input files.
}
 \item{folder}{Specifies the path of the folder where the files will be written in. If NA (DEFAULT) it uses the current working directory.
  }
  \item{percentage}{
assigns an equal percentage to every count where multiple counts are given (default=TRUE)
}

\item{fix.root}{
	It is possoble to fix the root node number for the analysis. One or more number can be given in a vector. If more than one number is given, the respective probabilities have to be added by hand in the freq.txt file, that is opened automatically by \code{ce.input}.
	
}
  \item{mainType}{
specfies the type of the analysis. If \code{maintype}=All_Models (Default) run analysis for each of the eight models.
If you want to optimize the parameters of one model, specify the model in mainType with: cr (CONST_RATE), crd (CONS_RATE_DEMI), crde (CONS_RATE_DEMI_EST), crnd (CONS_RATE_NO_DUPL); lr (LINEAR_RATE); lrd (LINEAR_RATE_DEMI), lrde (LINEAR_RATE_DEMI_EST) or lrnd (LINEAR_RATE_NO_DUPL). 
If you want to run simulation with fixed parameters do the same as for optimizing one model, adding fix_ to the model (i.e. fix_cr, fix_lrde...). Then you have to fix the parameter values by hand in the parameters file.
For details see the documentary on the program ChromEvol on http://www.tau.ac.il/~itaymay/cp/chromEvol/index.html

}
  \item{maxChrNum}{
defines the maximal chromosome number allowed in the analysis. Negative values (-X): Set the maximal chromosome number allowed to be X units larger than the maximal chromosome number observed in the data file
}
  \item{minChrNum}{
defines the minimal chromosome number allowed in the analysis. Negative values (-X): Set the minimal chromosome number allowed to be X units smaller than the minimal chromosome number observed in the data file
}

  \item{branchMul}{
If different than 1.0 then all branch lengths of the tree are multiplied by this scalar. Should be used if one of the model parameters are close to their boundary value (100), or in order to scale the tree when the branch lengths are exceptionally large or small.
}
  \item{rootAt}{
The internal node assumed to represent the root of the tree.
}
  \item{numberSim}{
The number of simulations for computing the expectation of the number of changes of certain transition type along each branch. Default: 1000 when mainType=All_Models, 10000 when one model is optimized.
Note: This step is computationally expensive. Lower values results in faster computations with decreased accuracy
}
  \item{ladderize}{
ladderizes the input tree and reorder node numbers. This operation cannot (yet) be done after the analysis.
}
}
\details{
\code{ce.input} writes three file cointaining the input tree, the input table and the parameters file as required by chromEvol.
It is recommended to format the tree, i.e. rooting, ladderizing, rotate branches, before creating the input files. It is not possible to do the necessary changes also on the output data.

}
\value{
A list including the input table and the input tree is returned
}
\references{
	http://www.tau.ac.il/~itaymay/cp/chromEvol/index.html
}
\author{
Natalie Cusimano <cusimano@bio.lmu.de>
}
\note{
Names in table and phylogeny must match!!!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{make.input.table}}, \code{\link{tree.length}}, \code{\link{read.cE.tab}}~~~
}
\examples{
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
