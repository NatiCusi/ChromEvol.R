\name{read.param}
\alias{read.param}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.param(..., optimize = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{optimize}{
%%     ~~Describe \code{optimize} here~~
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
function (..., optimize = FALSE) 
{
    models <- list(...)
    models <- unlist(models)
    SCAN <- list()
    param <- vector("list", 6)
    text <- c("LOSS_CONST", "GAIN_CONST", "DUPL", "HALF_DUPL", 
        "LOSS_LINEAR", "GAIN_LINEAR")
    for (i in 1:length(models)) {
        SCAN[[i]] <- scan(paste(models[[i]], "/chromEvol.res", 
            sep = ""), what = "list", sep = "\t")
        for (j in 1:6) {
            if (length(grep(text[j], SCAN[[i]])) > 0) 
                param[[j]][i] <- round(as.numeric(SCAN[[i]][grep(text[j], 
                  SCAN[[i]])[1] + 1]), digits = 2)
            else param[[j]][i] <- NA
        }
    }
    text2 <- c("lossC", "gainC", "dupl", "demi", "lossL", "gainL")
    names(param) <- paste("rate_", text2, sep = "")
    if (optimize) {
        ll <- gsub("LogLikelihood = ", "", SCAN[[1]][grep("LogLikelihood", 
            SCAN[[1]])])
        AIC <- gsub("AIC.*= ", "", SCAN[[1]][grep("AIC", SCAN[[1]])])
        param <- c(AIC = as.numeric(AIC), param)
        param <- c(logLikelihood = as.numeric(ll), param)
    }
    param
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
