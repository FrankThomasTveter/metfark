% comment
\name{highland(data,gpcol,ll)}
\title{Create selection vector for highland stations.}
\usage{
  nn <- highland(data,gpcol,thres,ll);
}
\arguments{
  \describe{
      \item{data}{Matrix.}
      \item{gpcol}{Geopotential column name in data.}
      \item{thres}{Threshold value gor geopotential.}
      \item{ll}{Optional: Old selection vector.}
   }
}
\value{Returns the selection vector for highland stations (gpcol > thres).}
\description{ Create selection vector for highland stations.}

\source{
   \url{https://www.github.com/FrankThomasTveter/metfark/farkRPackage}
}
\keyword{fark}
