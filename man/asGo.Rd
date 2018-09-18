\name{as.go}
\title{Functions to create and combine graphic data objects}
\alias{as.go}
\alias{as.go.0}
\alias{as.pgo}
\alias{as.lgo}
\alias{cgo}
\alias{cpgo}
\alias{clgo}
\alias{clgo.0}
\alias{ngo.0}
\alias{legend.go}
\alias{plotGo}
\alias{plotGo.0}
\alias{is.cpgo}
\alias{is.clgo}
\alias{is.lgo}
\alias{is.pgo}

\usage{
as.go(points=NULL, lines=NULL, xlab="", ylab="")

as.pgo(x,y=NULL,pch=1,col=1,cex=1,group=NULL,label=NULL)

as.lgo(x, y=NULL, col=1, lty=1, lwd=1, label=NULL)

cpgo(...)

clgo(...)

cgo(...)
}

\arguments{
  \item{x}{ vector of x coordinates of line or point data. Alternatively a 2
   column matrix containing the x and  y coordinates, or a list with
   components x,y and optionally col,lty,lwd,label as returned by
   \code{as.lgo()}, or col, pch, cex, group, label as returned by
   \code{as.pgo()}
 }
 \item{y}{ vector of y coordinates of line or point data. Only required
   if x is a vector.
 } 
 \item{col}{ vector of colour name or colour index, default black or 1
 }
 \item{cex}{ vector of size of point symbols, default 1}
 \item{pch}{ vector of plotting symbol number, default 1}
 \item{group}{ vector describing grouping of data} 
 \item{lty}{ line type name or index, default 1 }
 \item{lwd}{ integer value giving line width, default 1. }
 \item{label}{ text string to use as label for data in legend }
 \item{points}{ a points graphic object as returned by \code{as.pgo()}
  }
 \item{lines}{ a line graphic object as returned by
   \code{as.lgo()}. One of points or lines is required. }
 \item{xlab}{ optional x axis label, used to override the default
   extracted from the point or line graphic object. }
 \item{ylab}{ optional y axis label, used to override the default
   extracted from the point or line graphic object. }
}

\description{
  These functions allow graphic data objects to be created and
  joined. Graphic data objects comprise groups of x,y data of
  points and/or lines and associated plotting attributes.
  }
  
\details{
    Groups of point and line data are built into graphic objects using
    \code{as.go()} using objects returned by
    \code{as.pgo()} for points and \code{as.lgo()} for
    lines.
    
    Use \code{as.pgo()} to create point graphic objects. Pairs of x,y
    coordinates organized in labeled groups each with specific plotting
    attributes (symbol, symbol colour & symbol size).  The group argument can
    either be used in three ways. A single value in which case the x,y
    data is taken to be one group. A vector of the sizes of groups,
    thus group=c(3,5) means that the first 3 x,y pairs are in the first
    group and the second 5 values are in a second group. If the group
    vector is the same length as the x,y data it is taken as a vector of
    indices or labels of the group each x,y pair belongs in. The labels
    argument is similarly organized and should mirror the groups
    argument. Point graphic objects may be combined with cpgo(), which
    takes a list of point graphic objects to combine.

    Use as.lgo() to create line graphic objects. Pairs of x,y
    coordinates with specific plotting attributes (line type, line
    width, line colour) and group label. Line graphic objects may be
    combined with clgo(), which takes a list of line graphic objects to
    combine.

    Use as.go() to create graphic objects for use with the plot()
    function. as.go() creates a graphic object from a points graphic
    object, from a lines graphic object or a combination of points and
    lines. In all cases the xlab and ylab arguments may be supplied to
    override any defaults created from the data.

    Graphic objects may be combined using cgo() which takes a list of
    graphic objects.

    
  Whilst the returned object from as.go() or cgo() can be used in the
  conventional way with functions \code{\link{plot}} and
  \code{\link{legend}} for displaying the data and key to symbols,
  colours, line types etc. it is primarily designed for use in
  conjunction with \code{\link{figaro}}.
}

\value{
  as.pgo()and cpgo() return a point graphic object for use with cpgo() or
  as.go().

  as.lgo() and clgo() return a line graphic object for use with clgo()
  or as.go().
  
  as.go() and cgo() return a graphic data object of class 'go' containing the
  data to be plotted and methods (functions) for extracting the data and
  changing attributes of the data. The methods available are:
  \item{lines }{returns a list graphic object describing 
    the lines data and related attributes, 
    see \code{\link{as.lgo}}.}
  \item{points }{returns a list graphic object describing 
    the points data and related attributes, 
    see \code{\link{as.pgo}}.}
  \item{xlab }{ returns the x axis label }
  \item{ylab }{ returns the y axis label }
  \item{pset }{enables attributes of the points data to be changed }
  \item{lset }{enables attributes of the lines data to be changed }
}

\references{Full details are available on the Figaro web site \url{}}

\author{Colin Farrow (colinfarrow537@gmail.com) }

\note{If in doubt look in the source code.}

\seealso{ 
For an overview of Figaro graphical objects see \code{\link{figaro}}.
}

\examples{
\dontrun{
# create some data
x1 <- runif(10,0,10)
y1 <- runif(10,0,10)
x2 <- runif(10,5,15)
y2 <- runif(10,5,15)
# create two groups of data
xy1 <- as.pgo(x1,y1,col="red", pch=16, label="Group A")
xy2 <- as.pgo(x2,y2, col="blue", pch=17, label="Group B")
# combine as a graphic object and plot
xy <- as.go(cpgo(xy1, xy2),xlab="X", ylab="Y")
plot(xy)
# change the colours of points and redraw
xy$pset(col=c("yellow","green"))
plot(xy)


# some more data
a <- 0:360*pi/180
ys <-sin(a*a)
yc <-cos(a)
y2 <- ys+yc+5
# create three line graphic objects
sw <- as.lgo(a,ys,col="green", label="Sine(a^2)")
cw <- as.lgo(a,yc, col="red", lty="dotted", label="Cos(a)")
yw <- as.lgo(a,y2,col="blue",label="Cos(a) + Sin(a^2)")
# combine  into a graphic object
lp <- as.go(lines=clgo(sw,cw,yw),xlab="radians", ylab="amplitude")
# plot the result
plot(lp)

# combine the point and line data and plot
xylp <- cgo(xy, lp)
plot(xylp,xlab=\"x\", ylab=\"y\")
}
}


\keyword{aplot}
\keyword{internal}
\concept{Figaro}
