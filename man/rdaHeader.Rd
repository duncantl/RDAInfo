\name{rdaHeader}
\alias{rdaHeader}
\title{Get the meta-information from an RDA file/connection}
\description{
  This reads the metadata at the start of an RDA stream
  and returns the format version, etc.
}
\usage{
rdaHeader(con)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{the name of a file or a connection object}
}
\value{
  A list
  \item{format}{character string}
  \item{version}{integer value}
  \item{writer_version}{3-element numeric vector giving the major, minor and patch number for which
    version of R wrote the RDA content.}
  \item{min_reader_version}{3-element numeric vector  giving the major, minor and patch number for which
    version of R wrote the RDA file which can read this RDA content.}
  \item{native_encoding}{character string}
}
\references{
R Internals manual
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{load}},   \code{\link{save}}
  \code{\link{toc}},
}
\examples{
  f = system.file("sampleRDA", "test.rda", package = "RDAInfo")
  rdaHeader(f)
}
\keyword{IO}
