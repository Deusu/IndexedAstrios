\name{astrioIndex}
\alias{astrioIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
astrioIndex
}
\description{
A simple tool to convert "Sort parameter" of Beckman Coulter Astrios sorters to X-sort and Y-sort parameters. Requires as input a FlowFrame object.
}
\usage{
astrioIndex(ff, out_file = TRUE, out_name="Indexed_file.fcs")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ff}{
  A 'FlowFrame' object of the fcs file to be analysed.
  }
  \item{out_file}{
  Bolean indicating whether to export the indexed FlowFrame to a new fcs file. Default is TRUE.
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
Marco De Zuani, PhD
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{astrioIndex_fcs}}
}
\examples{
## Index the "fframe" flowframe and save to a new .fcs file
astrioIndex(ff = fframe, out_file = TRUE, file = "/output/indexed_file.fcs")

## Index the "fframe" flowframe" without saving a new file
indexed_fframe <- astrioIndex(ff = fframe, out_file = FALSE)
}

