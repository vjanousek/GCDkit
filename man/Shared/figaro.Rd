\name{FIGARO}
\alias{FIGARO}
\title{Figures as R objects}
\alias{plot.default}

\description{
A package of functions that provide an alternative way of creating and
modifying R figures. It comprises 3 elements: figure drawing objects,
figaro style sheets, and graphic objects.
}
\details{
R provides an excellent framework for producing graphics from data on a
number of different devices. Graphical functions included provide for a
range of diagrams to be created such as barplots, xy scatterplots
etc. Arguments to these functions may modify the appearance and calls to
other functions an add additional information ( eg text, lines,
points). Where more customization is required a new user written
plotting function may be created.

\bold{Figaro drawing objects.}
Figaro adds additional functionality to existing and user created
functions by providing a means to create figure objects which contain both
the data and methods to make changes to the figure. So for example the
title can be changed or the histogram fill colour altered and any
changes are automatically made visible on interactive devices. In
addition one can zoom in and out of the data.

Figaro objects currently permit the editing of the text, font, size and
colour of the  main title, sub title,and  axis labels; colour, size, and
symbol of points; colour, line type, and line width of lines.

Once a plot has been designed it may be reproduced on any of the
available R graphic devices. Thus figaro provides a degree of interactive
editing before committing to hardcopy.

Figaro objects may be saved using, dump(), or save() and restored at a
later time with source() or load() and the figure recreated exactly as
it was originally drawn.

Figure drawing objects can be used with the majority of the existing R
graphic functions and with user written ones.

\bold{Figaro style sheets} provide a way of describing the layout of
figures in a consistent fashion without the need to create separate
functions. Thus a style sheet might describe an xy scatterplot with
specific range of values on the x and y axes, the superimposition of a
grid and use of specific colours. Others may define labels to appear
in certain parts of the figure or arbitrary lines and polygons with or
without colour fill.

\bold{Graphic objects} describe one or more groups of point data, line
data or both, each with particular graphical attributes(colour, plot
symbol, size, line width, line colour, line style). Functions are
provide to create, combine and plot data in this class of objects.


Both figaro style sheets and graphic objects can be used independently of
the figure drawing objects, but when combined provides a powerful tool for
customizing figures. Plotting graphic objects with figaro enables
legends to be interactively placed and repositioned on a figure so as
not to obscure important data. Properties of graphical objects can be
customized on a per figure basis within a figaro object. Properties of
style sheet elements in a figure can be edited within a figaro drawing object.
}

\examples{
\dontrun{
# create a new figure object
p <- figaro(plot)
# plot some data
p$draw(runif(10), runif(10))
# change the main title
p$set(main="Figaro Demonstration")

# a histogram
h <- figaro(hist)
h$draw(rnorm(2000), col="blue")
# .. a better colour
h$set(col="yellow")

# using figaro with a style sheet
# create figure based on demo style from stylesheet 'fss'
d <- figaro(demo, fss="fss")
# draw a simple figure
d$draw(1:10)
# change the colour of the polygon 'pol1'
d$set("pol1", col="yellow")
# change the text
d$set("text", "Wow !", col='red', size=3. font=3)

# using figaro with a graphical object
# create some test data
x1 <- runif(10,0,10)
y1 <- runif(10,0,10)
x2 <- runif(10,5,15)
y2 <- runif(10,5,15)

# create two groups of point data
xy1 <- as.pgo(x1,y1,col="red", pch=16, label="Group A")
xy2 <- as.pgo(x2,y2, col="blue", pch=17, label="Group B")
# combine them
xy <- as.go(cpgo(xy1, xy2),xlab="X", ylab="Y")
# plot in the conventional way
plot(xy)
#plot in the figaro way
spot <- figaro(plot)
spot$draw(xy)
# change attributes of points
spot$set("points", col=c("khaki","green"))

# some more data
a <- 0:360*pi/180
ys <-sin(a*a)
yc <-cos(a)
y2 <- ys+yc+5
sw <- as.lgo(a,ys,col="green", label="Sine(a^2)")
cw <- as.lgo(a,yc, col="red", lty="dotted", label="Cos(a)")
yw <- as.lgo(a,y2,col="blue",label="Cos(a) + Sin(a^2)")
lp <- as.go(lines=clgo(sw,cw,yw),xlab="radians", ylab="amplitude")
#  draw using figaro
wave <- new(plot)
wave$draw(lp)
# .. and edit
wave$set(title="Adding Waveforms")

xylp <- cgo(xy, lp)
both <- new(plot)
both$draw(xylp)
both$set(title="Line and Point graphical object")
# add a legend,
#     select the location of the bottom left corner of the
#     legend box using the pointing device
both$legend()
both$set("lines", label=c("w1","w2", "w1 + w2"))
}
}


\seealso{ 
For graphical data objects, see \code{\link{as.go}}.
}

\keyword{hplot}
\keyword{iplot}
\keyword{aplot}
\keyword{color}
\keyword{internal}
\concept{Plot}
\concept{Figaro}
\concept{Menu: Plot editing}

\author{
Colin M. Farrow, \email{colinfarrow537@gmail.com}
}
