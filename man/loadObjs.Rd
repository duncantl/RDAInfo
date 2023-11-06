\name{loadObjs}
\alias{loadObjs}
\title{Version of \code{load} that returns the unserialized objects as a list}
\description{
  This is an alternative to \code{load} which
  does not unserialize the objects into an environment, typically the
  user's global environment, and overwrite existing variables.
  Instead, it returns the unserialized objects as a named list.
}
\usage{
loadObjs(file, ...)
}
\arguments{
  \item{file}{the RDA file or connection}
  \item{\dots}{additional arguments passed to \code{\link{as.list.environment}}}
}
\details{
  This uses \code{\link{load}} to unserialize the objects in the RDA stream
  into an empty environment and then converts that to a list and returns that.
}
\value{
  A list with named-elements
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{load}} and  \code{\link{Load}}  
}
\examples{
  f = system.file("sampleRDA", "test.rda", package = "RDAInfo")
  cur = ls()
  vals = loadObjs(f)
  now = ls()
  setdiff(now, cur)
  names(vals)
}
\keyword{IO}

