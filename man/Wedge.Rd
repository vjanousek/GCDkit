\name{Wedge}
\alias{Wedge}
\title{Wedge diagrams (Ague 1994)}

\section{Plugin}{Isocon.r}

\description{
    Implementation of Wedge diagrams after \emph{Ague (1994)} and 
    \emph{Bucholz and Ague (2010)} used for judging the mobility 
    of elements or oxides in course of various geochemically open-system  
    processes such as alteration or partial melting.
}

\usage{
Wedge(x = "Ti", y = NULL, protolith = NULL,
    outline = "chull", precision = 10, plotAltered = TRUE, 
    xmin = 0, ymin = 0, xmax = NULL, ymax = NULL, fun = NULL)
}

\arguments{
    \item{x}{a single geochemical species presumably immobile during the given rock transformation.}
    \item{y}{list of elements/oxides for plotting, separated by commas.}
    \item{protolith}{Boolean search pattern to specify the protolith samples in the data file.}
    \item{outline}{method for contouring the clusters of protolith and product compositions, see Details.}
    \item{precision}{precision of contours drawn, if  \code{'outline'="contour"}, see Details.}
    \item{plotAltered}{logical; should be the altered analyses plotted or just contoured?}
    \item{xmin, xmax}{(optional) limits for shared x axes of the individual plots.}
    \item{ymin}{(optional) minimum for all of the y axes of the plots.}    
    \item{ymax}{(optional) upper limits for each of the y axes of the plots.}
    \item{fun}{panel function to be applied to each of the individual plots.}
}

\details{
    Wedge diagrams \emph{(Ague 1994)} enable qualitative treatment of losses/gains 
    of geochemical species (elements or oxides) during open-system geological 
    processes, such as alteration, metamorphism or partial melting. 
    As such they represent a viable alternative to the isocon plots 
    \emph{(Grant 1986, 2005)} or concentration ratio diagrams \emph{(Ague 1994)}. 
    However, the Wedge diagrams have an advantage in that they take into account 
    the overall variability of the whole dataset (both of the putative 
    protolith and the altered product) and not just a selected whole-rock pair. 
    
    Wedge diagrams are simple binary plots of a potentially mobile 
    element \emph{j} versus a reference (immobile) element \emph{i}. 
    The compositionally heterogeneous protolith samples yield a cloud 
    of points. The outer edges of this cloud define a
    wedge-shaped region that converges towards the origin. 
    
    As shown by \emph{Bucholz and Ague (2010)}, the altered samples 
    that plot above and to the left of this wedge are thought 
    to have gained the mobile species \emph{j}, whereas those 
    falling below and to the right suffered its loss. 
    The samples that remain in the wedge but moved upwards are 
    thought to record residual enrichment, and those shifted downwards 
    to have underwent a residual dilution. 

    The samples defining the protolith variation can be selected based 
    on combination of three searching mechanisms 
    (by sample name/label, range or a Boolean condition) - see 
    \code{\link{selectSamples}} for details. 
    
    Implemented are two methods for outlining the 
    clusters of the protolith and altered compositions 
    (as specified by the argument \code{'outline'}), 
    convex hull \emph{(chull)} and contour \emph{(contour)}. For the latter,
    the shape of the contours drawn can be controlled using the parameter 
    \emph{(precision)}. The higher it is, the smoother contours result.  
    See \code{\link{contourGroups}} and \code{\link{chullGroups}} for further details.
        \if{html}{\figure{Wedge.png}} 
        \if{latex}{\figure{Wedge.png}{options: width=5.5in}}
    Optionally, the individual data points for the altered samples may be replaced
    by contours portraying their density, if \emph{plotAltered = FALSE}.
    
    Parameters \emph{xmin, xmax, ymin} and \emph{ymax} are passed to the function \code{\link{plotWithLimits}}
    used for the actual data plotting.
    
    Optionally, panel function specified by  \emph{fun} with two arguments, \emph{xlab} and  \emph{ylab},
    is applied to each of the plots. 
}

\value{
    Returns a matrix '\code{results}' of slopes of tie-lines from individual 
    protolith samples to the origin 
    (with a component for each diagram, i.e. for each species evaluated). 
    Lines of maximum and minimum slopes are those which are plotted as dashed lines, 
    thus defining the wedge of the protolith variation (see Details).
}

\section{Note}{
    This function uses the plates concept. The individual plots can be selected and their 
    properties/appearance changed as if they were stand alone Figaro-compatible plots. 
    See \code{\link{Plate}}, \code{\link{Plate editing}} and 
    \code{\link{figaro}} for details.
}

\seealso{
    \link{Ague}, \link{isocon}, \link{Plate},  \link{Plate editing}, \link{chull},  
    \link{contour} \link{contourGroups} \link{chullGroups}, \link{plotWithLimits}   
}


\references{ 
    Ague J J (1994) Mass transfer during Barrovian metamorphism of pelites, south-central 
    Connecticut; I, Evidence for changes in composition and volume. Amer J Sci 294: 989-1057
    \href{http://dx.doi.org/10.2475/ajs.294.8.989}{doi: 10.2475/ajs.294.8.989}
    
    Bucholz C E, Ague J J (2010) Fluid flow and Al transport during quartz-kyanite vein 
    formation, Unst, Shetland Islands, Scotland. J Metamorph Geol 28: 19-39
    \href{http://dx.doi.org/10.1016/0009-2541(67)90004-6}{doi: 10.1016/0009-2541(67)90004-6}
    
    Grant J A (1986) The isocon diagram - a simple solution to Gresens equation for 
    metasomatic alteration. Econ Geol 81: 1976-1982
    \href{http://dx.doi.org/doi:10.2113/gsecongeo.81.8.1976}{doi: doi:10.2113/gsecongeo.81.8.1976}

    Grant J A (2005) Isocon analysis: a brief review of the method and applications. 
    Phys Chem Earth (A) 30: 997-1004
    \href{http://dx.doi.org/10.1016/j.pce.2004.11.003}{doi: 10.1016/j.pce.2004.11.003} 

    Gresens R L (1967) Composition-volume relationships of metasomatism. 
    Chem Geol 2: 47-55
    \href{http://dx.doi.org/10.1016/0009-2541(67)90004-6}{doi: 10.1016/0009-2541(67)90004-6}

}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\examples{
data<-loadData("sazava.data",sep="\t")
Wedge("Ti","SiO2,FeOt,MgO,CaO,Na2O,K2O",
    protolith="Intrusion=\"Sazava\"","chull")
    
# Using the default precision of 10
Wedge("Ti","Zr,Nb,Sr,Rb,Ba",protolith="Intrusion=\"Sazava\"","contour")
     
Wedge("Ti","Zr,Nb,Sr,Rb,Ba",protolith="Intrusion=\"Sazava\"","contour",precision=100)  
}

\keyword{manip}
\concept{Calculation}
\concept{Alteration}
\concept{Plugin}
\concept{Plot}

