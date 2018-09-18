\name{gcdOptions}
\title{GCDkit options}
\alias{gcdOptions}

\description{
A graphical user interface (GUI, programmed in Tcl/Tk) for setting the main 
options controlling the behaviour of the GCDkit. 
}

\usage{
    gcdOptions(permanent.only=FALSE)
}

\arguments{
    \item{permanent.only}{logical; should be shown exclusively the option that can be set permanently?}
}

\details{
The settings are stored permanently in the file '\code{gcdkit.xxx}'
residing in the main GCDkit directory. They are loaded upon start
up. If  is missing or damaged, this file is created anew based on 
the default values. 

The panel connected to the function '\code{gcdOptions}' serves to change
several parameters. Most of them are passed to a list accessible in a way similar
to the standard R \code{\link{options}}. See the corresponding manual page 
for details and Examples for their implementation. Only a few are stored
in dedicated variables (see below).  

First, the default working directory can be set 
(and stored in the global variable \code{data.dir}). 

The parameter '\code{Minimize output on screen?}' is linked to the option 
\code{gcd.shut.up}. It controls excessive output to the Console window. 
Its default value is \code{FALSE}, meaning that detailed information is to be printed. 
This, however, may become not viable on slower systems and/or for extensive data 
sets.

The preferred precision of the numeric values that need to be rounded off are controlled 
by the parameter '\code{Precision of results}' (option \code{gcd.digits}). 

Using the parameter '\code{Plotting symbols magnification}', linked to the option 
\code{gcd.cex}, one can define a factor, by which are multiplied the plotting 
sizes defined for individual analyses upon startup and stored in the variable 
'\code{labels[,"Size"]}'. Please note that this is effective for the 
next plot if the GUI frontend is used to set this parameter, otherwise it will work for 
data files loaded from now on. 

In this way, the magnification is maintained proportional to the original sizes. 
If uniform plotting symbols sizes are desired, one should use the function 
\code{\link{setCex}} invoked from the menu

\code{Plot settings|Set uniform symbol size}.

The parameter '\code{Annotate fields in discrimination plots?}' toggles the 
labeling of the  fields on and off, typically for classification or 
geotectonic diagrams. It is stored in a logical variable \code{gcd.plot.text}, 
whose default is \code{TRUE}. The language for the field annotations can be 
selected using the list box connected to the option \code{gcd.language}. 

The next possibility is to alter the colours used, e.g, for texts or field boundaries
on diagrams. There are in total three colours stored in the list \code{plt.col}.
Alternatively, all the plots can be set to black and white (check box 
'\code{Set to BW?}' linked to the option '\code{gcd.plot.bw}'), excluding the 
data points. The default is \code{FALSE} (i.e. colour plotting).
    
The parameter '\code{Identify points?}' toggles on and off the 
identification/labelling of individual data points on plots. 
In general, the identification can be either interactive 
(option \cr 
\code{gcd.ident.each = TRUE}) or all the points can be labeled 
automatically as soon as the plotting is finished 
(option\cr 
\code{gcd.ident.each = FALSE}). 
In the former case, the user may click the left mouse button near 
the points to be identified, pressing the right mouse button 
when finished. 

The option \code{gcd.ident} determines whether identification should take 
place at all (the default value is zero, which means no 
identification). If the identification is on, the option \code{gcd.ident} attains
either 1 (identification by sample name), or the sequential number of the column 
in the data frame '\code{labels}' increased by one (identification by a label).

The identification by sample name for a current plot can be invoked also 
from the menu \cr
'\code{Plot editing|Identify points}'. There can be also 
chosen alternative means of points identification ('\code{Plot editing|Highlight multiple points}').
}

\value{
    Sets the following \code{options}:
    \item{gcd.plot.text}{logical; should be fields on classification diagrams 
        labeled by their names?}
    \item{gcd.language}{language for these labels.}
    \item{gcd.plot.bw}{logical; if TRUE, plots are produced as black and white.}
    \item{gcd.shut.up}{logical; determines whether extensive textual output 
        is to be printed.}
    \item{gcd.ident}{numeric; if zero, no identification takes place after plotting
        each diagram. If greater than zero, indicates the variable used to 
        identify individual data points. See Details.}
    \item{gcd.ident.each}{logical; are the data points to be identified individually?}    
    \item{gcd.digits}{preferred number of digits for rounding off the numeric values.}
    \item{gcd.cex}{a factor by which are multiplied all symbol sizes previously defined.}
    
    
    \if{html}{\figure{gcdOptions.png}} 
    \if{latex}{\figure{gcdOptions.png}{options: width=5.5in}}
    
    Remaining \code{\link{options}} changed by GCDkit which cannot be altered 
    via the GUI, though: 
    
    \item{prompt}{"GCDkit-> "}
    \item{windowsBuffered}{FALSE}
    \item{locatorBell}{FALSE}
    \item{scipen}{20} 
    \item{max.print}{99999999}
    
    If necessary they can be set directly in the file 
    '\code{gcdkit.xxx}'.    

    Apart from that the GUI panel sets the variables \code{data.dir} 
    (default data directory) and \code{plt.col} (colours for Figaro-compatible plots).
}

\seealso{
    \code{\link{options}}  \code{\link{identify}}  \code{\link{ID}}  \code{\link{figaro}}   \code{\link{setCex}} 
}

\examples{
bak <- options()                # backup the current options
options("gcd.ident"=1)          # identify by sample names
options("gcd.ident.each"=FALSE) # to label by sample names automatically, 
                                    # i.e. without the user interference
                            
plotDiagram("TAS",FALSE,FALSE)
options("gcd.ident"=0)          # to turn off the identification completely
options("gcd.plot.bw"=TRUE)     # to set the diagram to black and white
plotDiagram("TAS",FALSE,FALSE)
     
options("gcd.cex"=2)            # make the plotting symbols double as big 
                                    # (effective for the data files loaded from now on;
                                    # for immediate result use the GUI front end)
    
getOption("gcd.plot.bw")        # printing the current value of the given option
options(bak)                    # restore the previous options
}

\keyword{iplot}
\concept{Plot}
\concept{Parameter}
\concept{Menu: GCDkit}

\author{Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
