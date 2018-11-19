\name{to_iso}
\alias{to_iso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert countries to ISO codes}
\description{Return a vector of ISO country codes for a given vector of country names.}
\usage{
to_iso3(x, output="iso3")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{character vector of country names.}
  \item{output}{one of \code{c("iso3", "iso2", "clean")}. Determines output as either ISO-3 or ISO-2 codes, or a cleaned country name.}
}
\details{
The function includes a preset lookup table of unclean country names, clean country names, and corresponding ISO 3166-1 alpha-2 & alpha-3 codes (2 and 3-letter country codes). Input vectors are first cleaned, which includes removing punctuation and converting to lower-case. Then they are matched to the unclean country names. This step allows for differently-spelled country names to be reconcilled correctly. For example, \code{"United Kingdom"} and \code{"Great Britain"} will both be matched to the same codes.

Output can also be returned as a vector of "clean" country names. In the above example, both names would be matched to \code{"United Kingdom of Great Britain and Northern Ireland"}.

If an input value is not in the lookup table, a warning will notify of at which position the unmatched value is, and return the original value in that position in the output.
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
x = c("United States of America", "U.S.A.", "Canda", "Canada", "United Kingdom","Great Britain", "xyz")

to_iso(x, output="iso3")

to_iso(x, output="clean")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
