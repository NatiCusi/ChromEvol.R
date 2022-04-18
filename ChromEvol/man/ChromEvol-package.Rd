\name{ChromEvol-package}
\alias{ChromEvol-package}
\alias{ChromEvol}
\docType{package}
\title{
ChromEvol}
\description{
This package was written to facilitate the use of the program ChromEvol (Mayrose et al., 2010) for reconstruction of chromosome number evolution. It allows easy preparing of the input files, as well as the reading of the output files back into R. It offers several functions for visualizing and summarizing the results of the analyses.
}
\details{
\tabular{ll}{
Package: \tab ChromEvol\cr
Type: \tab Package\cr
Version: \tab 0.9-1\cr
Date: \tab 2013-11-13\cr
License: \tab GPL (>= 2)\cr
}
1. PREPARING INPUT DATA 
	
	\code{\link{ce.input}} reads a tree in newick format from file or from an R object of class phylo (\code{\link{ape}}) and a table from text file that can be either a table with two colums, 1st with species names, 2nd with haploid chromosome numbers or or in fasta format. 
			It writes the three or four from ChromEvol required input files in the specified or current directory.
	With \code{\link{read.cE.tab}} reads a chromEvol input table in fasta format and stores it as table in R, \code{\link{make.input.table}} creates a file with a species names (tip labels) of the tree, to add the chromosome numbers manually, so that it can be used as input table for \code{\link{ce.input}}.

2. READ DATA 
	
	With \code{\link{read.ce}} the results in the several output files are read and stored in an R object of class ChromEvol just by specifying the path of the folder of the analysis.

3. PLOT RESULTS
	
	Two functions allow you to immediately visualize the results just by specifying the ChromEvol object: 
	\code{\link{plot.anc}} plots the tree with pie charts indicating the probabilities of the inferred ancestral chromosome numbers at the nodes together with the number with the highest probability.
		\code{\link{plot.EVENTS}} plots the tree with the inferred events along the branches.
	Many arguments allow modifying and adjusting the figure to make it suitable for publication.	
	
	With \code{\link{extract.clade.ce}} and \code{\link{collapse.clade.ce}} a clade of interest can be extracted and then plotted or one or more clades can be collapsed, e.g. in large trees for matters of ease.
	
4. SUMMARIZE PARAMETERS
	
	There are several functions that summarize the results: i.e. by specifying the path of the folder, \code{\link{sum.Models}} summarizes the parameters of all models or only the best one(s), if the analysis was of type "All_Models". \code{\link{sum.Parameters}} summarizes the results of an R object of class ChromEvol, \code{\link{PP.nodes}} gives the probabilities of the inferred chromosome numbers at specified nodes. \code{\link{sum.EVENTS}}
}
\author{
Natalie Cusimano

Maintainer: Natalie Cusimano
}
\references{
Mayrose, I., M. S. Barker, and S. P. Otto. 2010. Probabilistic models of chromosome number evolution and the inference of polyploidy. Systematic Biology 59: 132–144.

\url{http://www.tau.ac.il/~itaymay/cp/chromEvol/index.html}
}
\keyword{ }
\seealso{

\code{\link{ape}}, \code{\link{phyloch}}
}
\examples{
~~ simple examples of the most important functions ~~
}
