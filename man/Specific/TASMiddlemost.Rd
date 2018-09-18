\name{TASMiddlemost}
\alias{TASMiddlemost}
\alias{TASMiddlemostPlut}
\alias{TASMiddlemostVolc}
\title{Middlemost's modification of TAS diagram}

\description{
Assigns data for Middlemost's modification of the TAS diagram into Figaro 
template (list '\code{sheet}') and appropriate values into '\code{x.data}' and 
'\code{y.data}'. 
}

\usage{
TASMiddlemostVolc()

TASMiddlemostPlut()
}

\details{
Middlemost's variation of classic IUGS-recommended TAS diagram, originally 
proposed by \emph{Le Bas et al. (1986)}. Boundaries of foidite, phonolite, 
trachyte, trachydacite and rhyolite fields are defined, as inferred from the 
phase relations in the TAS system. Moreover, the trachyte + trachydacite field 
is split into trachyte and trachydacite fields, silexite and sodalitite + 
nephelinolith + leucitolith fields are defined. 

    \if{html}{\figure{TASMiddlemostPlut.png}} 
    \if{latex}{\figure{TASMiddlemostPlut.png}{options: width=4.5in}} 
    
The same diagram layout is applied also to plutonic rocks as follows:
\tabular{ll}{
\bold{plutonic rocks}\tab \bold{volcanic rocks}\cr
\emph{Peridotgabbro} \tab \emph{Picrobasalt}\cr
\emph{Gabbro} \tab \emph{Basalt}\cr
\emph{Gabbroic Diorite} \tab \emph{Basaltic Andesite}\cr
\emph{Diorite} \tab \emph{Andesite}\cr
\emph{Granodiorite} \tab \emph{Dacite}\cr
\emph{Granite} \tab \emph{Rhyolite}\cr
\emph{Quartzolite} \tab \emph{Silexite}\cr
\emph{Monzogabbro} \tab \emph{Trachybasalt}\cr
\emph{Monzodiorite} \tab \emph{basaltic Trachyandesite}\cr
\emph{Monzonite} \tab \emph{Trachyandesite}\cr
\emph{Quartzmonzonite} \tab \emph{Trachydacite}\cr
\emph{Syenite} \tab \emph{Trachyte}\cr
\emph{Foid Gabbro} \tab \emph{Tephrite}\cr
\emph{Foid Monzodiorite} \tab \emph{Phonotephrite}\cr
\emph{Foid Monzosyenite} \tab \emph{Tephriphonolite}\cr
\emph{Foid Syenite} \tab \emph{Phonolite}\cr
\emph{Foidolite} \tab \emph{Foidite}\cr
\emph{Tawite/Urtite/Italite} \tab \emph{sodalitite/nephelinolith/leucitolith}
} 

    \if{html}{\figure{TASMiddlemostVolc.png}} 
    \if{latex}{\figure{TASMiddlemostVolc.png}{options: width=4.5in}} 
}

\value{
  \item{sheet}{list with Figaro Style Sheet data}
  \item{x.data}{SiO2 weight percent}
  \item{y.data}{Na2O+K2O weight percent}
}

\author{
Vojtech Erban, \email{vojtech.erban@geology.cz} 
}


\references{
Le Bas M J, Le Maitre R W, Streckeisen A & Zanettin B (1986)
A chemical classification of volcanic rocks based on the total alkali-silica
diagram. J Petrology 27: 745-750

Middlemost E A K (1994) Naming materials in the magma/igneous rock system. 
Earth Sci Rev 37: 215-224 
\href{http://dx.doi.org/10.1016/0012-8252(94)90029-9}{doi: 10.1016/0012-8252(94)90029-9} 
}

\seealso{
\code{\link{classify}}
\code{\link{TAS}}
\code{\link{Cox}}
\link{figaro}
\link{plotDiagram}
}

\examples{
#Within GCDkit, the plot is called using following auxiliary functions:
#To Classify data stored in WR (Groups by diagram)
classify("TASMiddlemostVolc")
# or
classify("TASMiddlemostPlut")

#To plot data stored in WR or its subset (menu Classification)
plotDiagram("TASMiddlemostVolc", FALSE)
# or
plotDiagram("TASMiddlemostPlut", FALSE)
}

\keyword{hplot}
\concept{Menu: Classification}
\concept{Classification}
\concept{Plot}
