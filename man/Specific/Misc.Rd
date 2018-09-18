\name{Misc}
\title{Miscellaneous geochemical indexes}
\alias{Misc}

\description{
Calculates a series of useful geochemical indexes.
}

\usage{
Misc(WR)
}

\arguments{
\item{WR}{a numerical matrix; the whole-rock data to be recalculated.}
}

\details{
Various petrochemical indexes are calculated, such as: 
\itemize{
\item total iron as \eqn{Fe_{2}O_{3}}{Fe2O3}
\item \eqn{Fe_{2}O_{3}}{Fe2O3}/FeO, \eqn{Na_{2}O}{Na2O}/\eqn{K_{2}O}{K2O} and 
\eqn{K_{2}O}{K2O}/\eqn{Na_{2}O}{Na2O} ratios
\item Larsen's DI - Differentiation index \emph{(Larsen 1938)}
\item Kuno's SI - Solidification index \emph{(Kuno 1959)}
\item Agpaitic index \emph{(Ussing 1912)}
}
}

\references{
Kuno H (1959) Origin of Cenozoic petrographic provinces of Japan and surrounding\cr 
provinces. Bull Volcanol 20: 37-76

Larsen E S (1938) Some new variation diagrams for groups of igneous rocks. 
J Geol 46: 505-520

Sorensen H (1997) The agpaitic rocks; an overview. Min Mag 61: 485-498

Ussing N V (1912) Geology of the country around Sulianehaab, Greenland. 
Meddr Grolnland, 38: 1-426
}

\value{
A numeric matrix '\code{results}'. 
}

\keyword{manip}
\concept{Calculation}
\concept{Edit}
\concept{Norm}
\concept{Menu: Calculations}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
