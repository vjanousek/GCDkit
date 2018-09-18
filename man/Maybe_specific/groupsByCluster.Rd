\name{groupsByCluster}
\title{Groups by cluster analysis}
\alias{groupsByCluster}

\description{
Grouping the data using the cluster analysis.
}

\usage{
    groupsByCluster(elems=
    "SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O",
    method="ave")
}

\arguments{
\item{elems}{numerical columns to be used for cluster analysis, typically major elements}
\item{method}{the agglomeration method to be employed. 
  This should be one of (or an unambiguous abbreviation thereof): 
    \code{'ward', 'single', 'complete',} 
    
    \code{'average', 'mcquitty', 'median', 'centroid'.}
    }
}

\details{
    After the dendrogram is drawn, the user is asked how many clusters is the dataset 
    to be broken into. 

    The vector containing the information on the current groups can be appended to 
    the data frame '\code{labels}'.

    The groups are initially numbered but this can be changed readily 
    using the function \code{\link{editLabFactor}}.

 
    For further details on the clustering algorithm, 
    see the R manual entry of '\code{hclust}'. 
}

\value{
    \item{groups}{character vector: the grouping information}
    \item{grouping}{set to zero.}
}

\seealso{
    \link{classify}  \link{groupsByLabel} \link{groupsByDiagram}
}

\keyword{manip}
\keyword{cluster}
\keyword{multivariate}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
