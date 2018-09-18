\name{figaro}
\alias{figaro}
\title{Create a new figaro drawing object}
\usage{
figaro(name, prefix="fss")
}
\arguments{
 \item{name}{ the name of an R function that creates a graphic figure,
   or the name of a figure definition from a figaro stylesheet, unquoted}
 \item{prefix}{ quoted name of a figaro stylesheet, default is "pss"}
}
\description{
 A figaro drawing object is created with methods for assigning data and
 for modifying many of the important graphical attributes.
}
\details{
Once created and data assigned to the figaro drawing object, one can
zoom/unzoom on the data and change attributes such as colour, symbol, and
titles. The output drawing can be recreated on any R
graphical device by a simple method call on the object.  Individual
custom changes to the drawing are automatically preserved.  Figaro drawing
objects can also be saved and restored in the default workspace or by using
\code{\link{dump}},\code{\link{save}} and \code{\link{load}}.
}
\value{
  The function returns a figaro drawing object with methods (functions)
  for creating and modifying the drawing. 
  \item{draw(...) }{ the draw() method assigns data to the figaro drawing
    object. The arguments are exactly as required by the graphic function
    used when the figure object is created. The data is drawn on the
    current graphical device.
    Used without any arguments draw will redraw the drawing on the
    current device. This way drawing objects can be created, saved and
    redrawn without needing to match the data and the function as
    required by conventional R graphics.
  }
  \item{set(...) }{ set() is used to change graphical attributes or
    characteristics of the drawing. Two argument forms are possible. The
    simplest is a list of attribute.name=
    
    value pairs, eg, set(main="Main Title", sub="subheading"). 
    If used in conjunction with a figaro style sheet, then to change 
    attributes of elements of the stylesheet
  the name of the stylesheet graphic element must be given as the first
  argument, eg. set("txt1", font=3) to change the font of the txt1
  element of the diagram. Any changes made are automatically stored and
  the figure redrawn.
}
  \item{zoom() }{zoom() requires the user to pick two coordinates with
    the pointing device at opposite corners of the area of the drawing
    of interest. The figure will be redrawn using these coordinates to
    from the bounding box of the plot. Multiple calls to zoom() are
    permitted.
  }
  \item{unzoom() }{unzoom() will restore the drawing to view all the data
    as displayed before any zoom() calls.
  }
  \item{legend() }{The legend function will allow the placement under user
  control of a legend. The location  of the bottom left corner of the
  legend should be selected with the pointing device. legend() requires
  a graphic data object, describing the data and labels to use, see
  \code{\link{as.go}}. For example see \code{\link{FIGARO}}.
  }
  \item{show() }{show() is a debugging aid  for displaying the data in
    the figaro object }
}

\references{ Full details are available on the Figaro web site \url{} }
\author{ Colin Farrow (colinfarrow537@gmail.com) }
\note{ If in doubt look in the source code. }

\seealso{ For an overview of figaro see \code{\link{FIGARO}},
  graphical data objects, see \code{\link{as.go}}.
  Also \code{\link{par}} for details of graphical parameters that may
  be changed to customize a drawing, but note that some graphical
  parameters (those concerning layout etc ) are intended for use in the
  underlying graphical function rather than in a figaro graphic.
  }
  

\examples{
# create a new figure object
p <- figaro(plot)
# plot some data
p$draw(runif(10), runif(10))
# change the main title
p$set(main="Figaro Demonstration")
# and take a closer look
p$zoom() 
# then use the pointer to select two points at opposite corners 
# of the area of interest

# a histogram
h <- figaro(hist)
h$draw(rnorm(2000), col="blue")
# .. a better colour
h$set(col="yellow")

# save as postscript
postscript(file="example_plots")
p$draw()
h$draw()
dev.off()
}

\keyword{hplot}
\keyword{iplot}
\keyword{aplot}
\keyword{color}
\keyword{internal}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}
