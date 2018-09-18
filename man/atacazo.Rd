\encoding{UTF-8}
\name{atacazo}
\docType{data}
\alias{atacazo}

\title{Whole-rock composition of lavas from the Atacazo and Ninahuilca volcanoes, Ecuador}
\description{
    This data set gives the whole-rock major- and trace-element contents, together with Sr and Nd isotopic 
    compositions of lavas from two volcanic complexes in Ecuador: the Atacazo and the Ninahuilca 
    \emph{(Hidalgo, 2006; Hidalgo et al., 2008)}. This dataset is used in a worked example (chapter 25) 
    of \emph{Janousek et al.}'s book (2016).
	
	Note that this data set contains information on symbols and colours to be used in \emph{GCDkit}, 
    as well as labels (\code{Volcano}) that can be used for grouping or similar purposes. 
    It also includes \code{87Sr/86Sr} and \code{143Nd/144Nd}. Therefore, if the SrNd plugin 
    for \emph{GCDkit} is installed, these columns will automatically be recognized as Sr and Nd initial 
    isotopic ratios when loading it into \emph{GCDkit} (via \code{accessVar("atacazo")}, 
    allowing variables such as TDM to be calculated and isotope-based diagrams to be plotted. 
    As no \code{Age} column is supplied, the user will be prompted for the emplacement age; 
    the volcanoes being Quaternary in age (220-71 ka for Atacazo and 71-2 ka for Ninahuilca), 
    the age correction is insignificant and a small value (of 0.1 for instance) is adequate.
}

\usage{
data(atacazo)
}

\format{A data frame containing 110 observations of 38 variables.}

\examples{
data(atacazo)
accessVar("atacazo")
binary("SiO2","Ba")
ageEps()
}

\source{
data by Silvana Hidalgo, \email{shidalgo@igepn.edu.ec},
 
formatted by Jean-François Moyen, \email{jfmoyen@gmail.com}
}

\references{
Hidalgo S (2006) Les interactions entre magmas calco-alcalins "classiques" et adakitiques: 
exemple du complexe volcanique Atacazo- Ninahuilca (Equateur). 
Unpublished PhD thesis, Université Blaise-Pascal, Clermont-Ferrand, France

Hidalgo S, Monzier M, Almeida E, Chazot G, Eissen JP, van der Plicht J, Hall M (2008) 
Late Pleistocene and Holocene activity of the Atacazo-Ninahuilca Volcanic Complex (Ecuador). 
J Volc Geoth Res 176: 16-26 
\href{https://doi.org/10.1016/j.jvolgeores.2008.05.017}{doi: 10.1016/j.jvolgeores.2008.05.017}

Janousek V, Moyen JF, Martin H, Erban V, Farrow CM (2016) Geochemical Modelling of Igneous 
Processes - Principles and Recipes in the R Language. Springer Verlag, Berlin 
\href{http://www.springer.com/la/book/9783662467916}{isbn: 978-3-662-46792-3}
}

\keyword{datasets}
