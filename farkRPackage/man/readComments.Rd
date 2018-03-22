% comment
\name{readComments(filename,name)}
\title{Read comments from table file.}
\usage{
  legs <- readComments("file.table", "LEGENDS");
  attr <- readComments("file.table", "ATTRIBUTES");
  cols <- readComments("file.table", "COLUMNS");
}
\arguments{
   \describe{
      \item{filename}{Name of table file.}
      \item{type}{type of comments: "LEGENDS","ATTRIBUTES","COLUMNS"}
   }
}
\value{Returns an array of the data found.}
\description{ Read comments from table file.}

\source{
   \url{https://www.github.com/FrankThomasTveter/metfark/farkRPackage}
}
\keyword{fark}
