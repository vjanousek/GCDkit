\name{Export to HTML tables}
\title{Export to HTML tables}
\alias{HTMLTableMain}
\alias{HTMLTableWR}
\alias{HTMLTableResults}
\alias{HTMLtableOrdered}

\description{
    Outputs the specified data with (optional) labels into HTML. 
    This format is useful for importing into spreadsheets, word processors or 
    publishing on the WWW.
}

\usage{
    HTMLTableMain(what,digits=2,desc=NULL,title=" ",sum.up=FALSE,open=TRUE,
    close=TRUE,browse=TRUE,filename=paste(data.dir,"R2HTML/htmltable",sep="/"),
    rotate=FALSE)

    HTMLtableOrdered(what,which=rownames(what),labs=labels,digits=2,desc=NULL,
    title=" ",sum.up=FALSE,key1=NULL,key2=NULL,
    filename=paste(data.dir,"R2HTML/htmltable",sep="/"),split.by=25,rotate=TRUE) 

    HTMLTableWR(filename="htmltable")

    HTMLTableResults(filename="htmltable")
}

\arguments{
  \item{what}{numeric matrix; data to be exported}
  \item{digits}{required precision}
  \item{desc}{name of the columns within 'labels' to be attached to the table}
  \item{title}{main title}
  \item{sum.up}{logical; should be a sum calculated?}
  \item{open}{logical; should be opened a new HTML file?}
  \item{close}{logical; should be the HTML file closed when finished?}
  \item{browse}{logical; should be the HTML file finally opened in the default browser?}
  \item{filename}{optional name for the file produced}
  \item{rotate}{logical, should be the table transposed, with samples in columns and variables in rows?}
  \item{which}{(optional) sample names in numeric matrix 'what' for the output}
  \item{labs}{name of variable with textual labels}
  \item{key1}{is a variable in numeric matrix 'what'}
  \item{key2}{is a grouping information (name of a column in 'labs')}
  \item{split.by}{maximal number of data columns per page}
}

\details{
\code{HTMLTableWR} and \code{HTMLTableResults} are GUI front ends to 
\code{HTMLTableMain}, the former enabling the user to choose 
samples (rows) and columns for the output using the searching mechanisms common 
in the GCDkit. 

\code{HTMLTableWR} outputs the numeric data (with optional labels and sum) 
stored in the data matrix \code{'WR'}.

\code{HTMLtableOrdered} also outputs the numeric data stored in the numeric matrix specified 
by parameter \code{'what'}. Optional argument \code{'which'} gives the list of sample names (rows)
in the matrix to be saved. The data are first sorted based on \code{'key2'}, which typically gives  
a grouping information (name of a column in \code{'labs'}). Within each of the groups, the data
are further sorted based on the numeric variable \code{'key1'}. See example. 

\code{HTMLTableResults} outputs the results of the most recent calculation 
(with optional labels and sum) as stored in the variable \code{'results'}.

The plugin attempts to format sub- and superscripts in the names of variables.
 
The created file \code{'filename'} is placed in the subdirectory \code{'R2HTM'} 
of the current working directory; when finished, it is previewed in a browser.
The style for the table is determined by the cascade style file \code{R2HTML.css} 
in the subdirectory \code{'Plugin'}.

\if{html}{\figure{htmltable.png}} 
\if{latex}{\figure{htmltable.png}{options: width=3in}}

}

\value{
None.
}

\section{Warning}{
The plugin uses R2HTML library, which must be downloaded from CRAN and properly 
installed. Its presence is checked before the code is executed.
}

\examples{
# Works on the 'sazava' test data set
setwd(paste(gcdx.dir,"Test_data",sep="/")) 
loadData("sazava.data")

HTMLTableMain(WR[,c("SiO2","MgO","FeOt")],digits=2,desc="Intrusion",title="Sazava [wt.\%]")

HTMLtableOrdered(WR[,LILE],digits=1,key1="SiO2",key2="Intrusion",title="Large Ion Lithophile
    Elements (ppm)",split.by=3)
}

\keyword{file}
\concept{File}
\concept{Print}
\concept{Menu: Data handling}
\concept{Menu: Calculations}

\author{
The R2HTML package was written by Eric Lecoutre.
 
Vojtech Janousek, \email{vojtech.janousek@geology.cz}}
