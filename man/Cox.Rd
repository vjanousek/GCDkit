\name{Cox}
\title{TAS diagram (Cox et al. 1979)}
\alias{Cox}
\alias{CoxPlut}
\alias{CoxVolc}

\description{
Assigns data for Cox's diagram into Figaro template (list '\code{sheet}') and                                   
appropriate values into '\code{x.data}' and '\code{y.data}'. 
}

\usage{
CoxVolc(alkline=TRUE)

CoxPlut(alkline=TRUE)
}

\arguments{
\item{alkline}{Logical: Should the boundary between alkaline and subalkaline 
rocks (Irvine & Baragar 1971) be drawn?}
}

\details{
TAS diagram, as proposed by \emph{Cox et al. (1979)} for volcanic rocks and 
adapted by \emph{Wilson (1989)} for plutonic rocks.

For volcanic rocks, the following diagram is plotted:
    \if{html}{\figure{Coxvolc.png}} 
    \if{latex}{\figure{Coxvolc.png}{options: width=4in}} 
    
And the version for plutonic rocks contains the following fields:

\tabular{ll}{
\bold{volcanic rocks} \tab \bold{plutonic rocks}\cr
\emph{basalt} \tab \emph{gabbro}\cr
\emph{basaltic andesite} \tab \emph{undefined}\cr
\emph{andesite} \tab \emph{diorite}\cr
\emph{dacite} \tab \emph{quartz diorite (granodiorite)}\cr
\emph{rhyolite} \tab \emph{alkali granite/granite}\cr
\emph{hawaiite} \tab \emph{gabbro}\cr
\emph{trachyandesite} \tab \emph{undefined}\cr
\emph{basanite/tephrite} \tab \emph{undefined}\cr
\emph{mugearite} \tab \emph{syeno-diorite}\cr
\emph{benmorite} \tab \emph{syenite}\cr
\emph{trachyte} \tab \emph{syenite}\cr
\emph{nephelinite} \tab \emph{ijolite}\cr
\emph{phonology nephelinite} \tab \emph{undefined}\cr
\emph{phonolitic tephrite} \tab \emph{undefined}\cr
\emph{phonolite} \tab \emph{nepheline syenite}\cr
}
  
    \if{html}{\figure{Coxplut.png}} 
    \if{latex}{\figure{Coxplut.png}{options: width=4in}}
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{SiO2 weight percent}
  \item{y.data}{Na2O+K2O weight percent}
}



\references{
Cox K G, Bell J D & Pankhurst (1979) The Interpretation of Igneous Rocks. 
Allen & Unwin, London

Wilson M (1989) Igneous Petrogenesis. Chapman & Hall, London

Irvine T M & Baragar W R (1971) A guide to the chemical classification of 
common volcanic rocks. Canad J Earth Sci 8: 523-548
\href{http://dx.doi.org/10.1139/e71-055}{doi: 10.1139/e71-055} 
}

\author{
  Vojtech Erban, \email{vojtech.erban@geology.cz}\cr 
  & Vojtech Janousek, \email{vojtech.janousek@geology.cz}
}

\seealso{
  \link{classify}
  \link{figaro}
  \link{plotDiagram}
}

\examples{
#TAS diagram is called using following auxiliary functions:
#Classifies data stored in WR (Groups by diagram)
classify("CoxVolc")
#or
classify("CoxPlut")

#Plots data stored in WR or its subset (menu Classification)
plotDiagram("CoxVolc", FALSE)
#or
plotDiagram("CoxPlut", FALSE)
}

\keyword{hplot}
\concept{Plot}
\concept{Grouping}
\concept{Edit}
\concept{Menu: Data handling}
\concept{Menu: Classification}
