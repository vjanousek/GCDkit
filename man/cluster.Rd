\name{cluster}
\title{Statistics: Hierarchical clustering}
\alias{cluster}

\description{
Hierarchical cluster analysis on a set of dissimilarities.
}

\usage{
cluster(elems = "SiO2,TiO2,Al2O3,FeOt,MnO,MgO,CaO,Na2O,K2O", 
    method = "average")
}

\arguments{
  \item{elems}{numerical columns to be used for cluster analysis, typically major 
  elements}
  \item{method}{
    the agglomeration method to be employed. 
    This should be one of (or an unambiguous abbreviation thereof): 
    \code{'ward', 'single', 'complete',} 
    
    \code{'average', 'mcquitty', 'median', 'centroid'.}
    }
}

\details{
The samples can be selected based on combination of three searching mechanisms 
(by sample name/label, range or a Boolean condition) - see 
\code{\link{selectSamples}} for details. 

Even though a list of major elements is assumed as a default, different 
variables can be specified by the function '\code{\link{selectColumnsLabels}}'.

    
The user is also asked to specify a label for the individual samples, 
default are their names.

After the dendrogram is drawn, the individual clusters can be identified. 
For each sample falling into the given group, specified information 
(e.g. Locality, Rock Type and/or Author) can be printed.

For further details on the clustering algorithm, see the R manual entry of 
'\code{hclust}'. 
}

\value{
 None.
}

\section{Warning}{
Names of existing numeric data columns and not formulae involving these can be 
handled at this stage. Only complete cases are used for the cluster analysis. 
}

\seealso{
'\code{\link{hclust}}'
}

\keyword{hplot}
\keyword{cluster}
\keyword{multivariate}
\concept{Plot}
\concept{Statistics}
\concept{Menu: Calculations: Statistics}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
