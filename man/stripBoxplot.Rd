\name{stripBoxplot}
\Rdversion{1.1}
\alias{stripBoxplot}

\title{
    Statistics: Stripplot by groups - with boxplots
}

\description{
    Stripplot for selected variable, respecting the grouping. Each of the 
    stripplots for the individual groups are underlain by a boxplot, so that the median, 
    quartiles and range are immediately apparent. 
    Optionally, the data points can be replaced by 
    variously sized/coloured circles, depicting a distribution of a second variable. 
}

\usage{
stripBoxplot(yaxis="",zaxis="0",ymin=NULL,ymax=NULL,pal="heat.colors",ident=FALSE,
    scaling.factor=NULL,boxplot.data=NULL,
    pch=NULL,col=NULL,cex=NULL,silent=TRUE,add=FALSE)
}

\arguments{
    \item{yaxis}{specification of the variable used for stripplots/boxplots.}
    \item{zaxis}{(optional) specification of the variable depicted by the circles.}
    \item{ymin, ymax}{minimum and maximum of the y axis.}
    \item{pal}{name of predefined palette.}
    \item{ident}{logical; should be the samples identified interactively after plotting?}
    \item{scaling.factor}{numeric; relative size of the plotted symbols.}
    \item{boxplot.data}{a list; data for the underlying boxplots 
        (if different from those used for the stripplots). See Details.}
    \item{pch}{plotting symbols.}
    \item{col}{plotting colours.}
    \item{cex}{relative size of the plotting symbols.}
    \item{silent}{logical, should be some of the above parameters chosen by the appropriate dialogues?}
    \item{add}{logical; should be the diagram added to a preexisting plot 
        (rather than a new plotting window opened)?}
}

\value{
    None.
}

\details{
    Stripplot shows 1D scatter plots for each of the groups, with some artificial 
    noise (jitter) added to make the individual points better visible. Stripplots 
    are a good alternative to boxplots when sample sizes are small.

    If no variable is specified as an argument 
    '\code{yaxis}', and the function is invoked in interactive regime (\code{silent = FALSE}), 
    the user can enter it using the function '\code{\link{selectColumnLabel}}'. 

    If '\code{zaxis}' is zero, assigned plotting symbols, colours and symbol
    sizes are used. 

    If '\code{zaxis}' refers to a valid variable name, the data points are shown as
    circles, the size and colours of which correspond to this second variable. In the batch mode,
    the relative size of the circles plotted can be specified using the parameter 
    \code{scaling.factor}.

    \if{html}{\figure{stripBoxplot.png}} 
    \if{latex}{\figure{stripBoxplot.png}{options: width=5.5in}} 

    In the specification of the variable(s) can be used also 
    arithmetic expressions, see \code{\link{calcCore}} for the correct syntax.

    The colour scheme can be specified by '\code{pal}'. The legal colour schemes are: 
    \code{"grays","reds","blues"},\cr
    \code{"greens","cyans","violets","yellows","cm.colors","heat.colors","terrain.colors"},
    \code{"topo.colors","rainbow"} and \code{"jet.colors"}.
    
    Normally, the stripplots are underlain by boxplots portraying the statistical distribution 
    of the same data, as used for construction of stripplots for each of the groups. 
    However, with caution, one  can specify via \code{boxplot.data} a list containing the 
    alternative data to be shown on background. Clearly, the number of components in the list, 
    as well as their order, needs to exactly match the individual groups (the levels).   
}

\seealso{
\code{\link{stripplot}}, \code{\link{boxplot}}, \code{\link{strip}}, \code{\link{plotWithCircles}}
}

\examples{
    stripBoxplot("(Na2O+K2O)/Al2O3")
}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}

\keyword{hplot}
\keyword{univar}
\concept{Plot}
\concept{Menu: Calculations: Statistics}
\concept{Statistics}
\concept{Grouping}
