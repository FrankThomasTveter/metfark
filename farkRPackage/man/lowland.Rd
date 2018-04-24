% comment
\name{lowland(data,gpcol,ll)}
\title{Create selection vector for lowland stations.}
\usage{
  nn <- lowland(data,gpcol,thres,ll);
}
\arguments{
  \describe{
      \item{data}{Matrix.}
      \item{gpcol}{Geopotential column name in data.}
      \item{thres}{Threshold value gor geopotential.}
      \item{ll}{Optional: Old selection vector.}
   }
}
\value{Returns the selection vector for lowland stations (gpcol < thres).}
\description{ Create selection vector for lowland stations.}

\source{
   \url{https://www.github.com/FrankThomasTveter/metfark/farkRPackage}
}
\keyword{fark}
