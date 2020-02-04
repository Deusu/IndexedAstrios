\name{astrioIndex_fcs}
\alias{astrioIndex_fcs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
astrioIndex_fcs
}
\description{
A simple tool to convert "Sort parameter" of Beckman Coulter Astrios sorters to X-sort and Y-sort parameters. Requires as input an fcs file.
}
\usage{
astrioIndex_fcs(fcs, out_file = TRUE, out_name="Indexed_file.fcs")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fcs}{
  Name (and location) of the fcs file to be indexed.
  }
  \item{out_file}{
  Bolean indicating whether to export the indexed FlowFrame to a new fcs filwe. Default is TRUE.
  }
  \item{out_name}{
  Locatiion and name for the file to be exported. By default it exports in the working directory as "Indexed_file.fcs"
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
