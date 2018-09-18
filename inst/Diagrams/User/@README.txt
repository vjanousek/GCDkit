This is a subdirectory for templates of user-defined diagrams, either stand-alone, or their plates. 

All *.r or *.R files stored in this subdirectory, except those whose name starts with underscore ("_") are sourced every time the menu item 'Plots|User defined...' is invoked. Hence the list of available diagrams is always built on fly, using the function .userlist().
The diagrams can be both stand-alone, or plates. So far, the user-defined diagrams cannot be used for classification and cannot have language versions other than English. 
For stand-alone, single Figaro templates, the crucial information is:
•	Name of the plot: sheet$demo$template$GCDkit$plot.name
•	Sequence number of the plot in the menu: sheet$demo$template$GCDkit$plot.position
•	Name of function defining a diagram
For a plate of Figaro templates:
•	Name of the plot: plate$plot.name
•	Sequence number of the plot in  the menu: plate$plot.position
•	Name of function defining a plate 

In both cases, the file should define a single function of a name identical to the (root of) the filename. 
 'Sequence number' above is any positive real number (i.e., does not have to be an integer).

HTML documentation is linked to the help system automatically. However, this is only done when the *.htm file, named exactly  like the function, resides in Diagrams/User/doc subdirectory. See examples provided with the current distribution.
