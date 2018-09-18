###############################
#                             #
#  Welcome to GCDkit for Win  #
#                             #
###############################
Note that recommended systems to run GCDkit are RWindows Vista/7/8/10, complete function/stability under 2000/NT/XP cannot be guaranteed.
The support for Win 95/98/ME has been (long) discontinued.

=======================
#    Installation     #
=======================

1. Download *appropriate* version of R for Windows 
--------------------------------------------------
o You can download R either from the CRAN site at http://cran.r-project.org/ or visit http://gcdkit.org/.

o IMPORTANT!  Gain the Administrator rights.

o Run the executable file and select the required items as well as the target directory (thereafter referred to as %RHOME%).

o IMPORTANT! The current version of GCDkit, 4.1, has been developed in R 3.2.1. The function under a different version of R cannot be guaranteed. 

2. Download GCDkit
------------------
o Download GCDkit from http://www.gcdkit.org

3. Install GCDkit 
-----------------
o IMPORTANT! Gain the Administrator rights.

o Double click the file GCDkit4.0_for_R3.2.1.exe and follow instructions.  

o NOTE that installation path of the system itself is %RHOME%\library\GCDkit and cannot be changed. 

o The rest of the directory structure, program files group and shortcut will be created automatically.

4. Running
----------
Double click the shortcut on the desktop and R should start loading the GCDkit on fly.


===========================
#     Troubleshooting     #
===========================

• WARNING: DO NOT DELETE the file '.Rprofile' residing in the main GCDkit directory.
Otherwise desktop shortcuts to run the GCDkit will stop working!


* Setup does not start at all or is extremely slow
--------------------------------------------------
This may happen with some resident antivirus programmes, such as F-Secure on my old PC. 
Please switch off the resident protection temporarily. 

* Creating a shortcut to the RGUI.exe on your desktop (if the automatic install fails)
--------------------------------------------------------------------------------------
Specify the main directory, where RGUI.exe resides (%RHOME%\bin), and the directory with GCDkit data.

It can look something like:
Target: C:\R\R-3.2.1\bin\i386\Rgui.exe --silent --no-save --sdi

Or (64bit version)
Target: C:\R\R-3.2.1\bin\x64\Rgui.exe --silent --no-save --sdi

Please note that in the 64-bit version, RODBC library does not function properly and thus the import/export from/to Excel, Access and DBF are not available.


Starting directory: "C:\R\R-3.2.1\library\GCDkit" (or such alike)

* Any diagram based on plate concept fails to be drawn (e.g., multiple plots such as Harker plots)
--------------------------------------------------------------------------------------------------
Make sure that R runs in SDI mode, i.e., that each of the plotting windows is independent and can be moved about your desktop freely. 


* The graphic windows fail to redraw, especially if too many of them are being open. 
Some of the functions/the whole system not working.
-------------------------------------------------------------------------------------- 
While working, try to keep the number of open graphical windows to a necessary minimum), as the R may become instable, failing to redraw graphical windows if too many of them are being open. It is always a good idea to close the unnecessary ones, for instance using the function graphicsOff() or the corresponding item from the menu GCDkit.

If necessary, report the bug. For doing so, make sure that you send maximum information, i.e. attach your dataset, describe us in length what version of system, R and GCDkit you are using and what exactly happened (or did not happen ;-)). 

Enjoy!

Vojtech Janousek and others

vojtech.janousek@geology.cz
10 February 2016
---------------------------
http://www.gcdkit.org
