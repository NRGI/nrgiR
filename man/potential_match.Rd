\name{potential_match}
\alias{potential_match}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find potential matches for a vector against another}
\description{
Given a vector, find potential matches for each item against another vector. Items can also be grouped to check only against items in the same grouping.
}
\usage{
potential_match(vec1, group1=NULL, vec2, group2=NULL, fancy=FALSE, agrep_value=.3, adist_value=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec1}{character vector.}
  \item{group1}{character vector, of the same length as \code{vec1}.}
  \item{vec2}{character vector.}
  \item{group2}{character vector, of the same length as \code{vec2}.}
  \item{fancy}{if \code{TRUE}, use \code{agrep} to determine approximate matches. If \code{FALSE}, use \code{adist} to determine approximate matches.}
  \item{agrep_value}{\code{max.distance} argument to \code{agrep}. Used when \code{fancy==FALSE}. See \code{\link[base]{agrep}}.}
  \item{adist_value}{Maximum Levenshtein distance between strings allowed for results. Higher values allows for less similar strings to be returned as potential matches. Used when \code{fancy==TRUE}. See \code{\link[utils]{adist}}.}
}
\details{

}
\value{
A \code{data.table}. The first two columns are the group (if applicable) and original character strings matched from \code{vec1}. Columns three through eight contain up to six potential matches for the group/string pair in columns one and two. Where fewer than six potential matches are found, the rightmost columns will contain \code{NA}.
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

potential_match(vec1=c("abc", "pigglywiggly"), vec2=c("abc", "rtyert", "piggly"), fancy=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}% use one of  RShowDoc("KEYWORDS")
\keyword{}% __ONLY ONE__ keyword per line
