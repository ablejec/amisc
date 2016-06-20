##############################################################
##
## RUSER private functions
##
## A Blejec
##
## $Revision: $
## $Date: $
##
##############################################################
#library(xlsReadWrite)   ## for obvious reasons
#library(Hmisc)
#library(gtools)         ## for function: running
##
catln <-
  function(...) {
    cat(..., "\n")
  }
#
testing <-
  function(...) if (.testing) cat(" > > > ", deparse (substitute (...)), " = ", ...,"\n")
#
options(keep.source = TRUE)
#
read.clipboard <-
  function (header = T, sep = "\t", ...) {
    read.table (file = "clipboard", header = header, sep = sep, ...)
  }
##########
.prn <- F
savePlot <-
  function(file = "Myplot%02d", prn = NULL , ...) {
    if (! is.null (prn)) .prn = prn
    if (.prn) {
      grDevices::savePlot(file = file, ...)
      cat("Plot saved to:", file, "\n")
    }
    else
      cat("Plot ", file, "not saved, .prn = ", .prn)
  }
##########
savePDF <-
  function(file = "Myplot%02d.pdf", comment = "", path = "", height = 6, width = 8, prn = NULL, ...) {
    if (! is.null (prn)) .prn = prn
    if (.prn) {
      ## check if type is present and use .pdf if not
      if (which (substring(paste (file, ".", sep = "", collapse = ""), 1:(nchar (file) + 1), 1:(nchar (file) + 1)) == ".")[1] > nchar (file)) file = paste (file, "pdf", sep = ".")
      pdf (paste (path, file, sep = "", collapse = ""), height = height, width = width, ...)
      dev.set(dev.prev())
      dev.copy(which = dev.next())
      dev.off ()
      ## place LaTeX lines into Logfile
      lfn <- file ("Logfile.Tex", open = "a")
      catln("%%", file = lfn)
      catln("\\begin{frame}", file = lfn)
      catln("\\frametitle{", comment, "}", file = lfn)
      catln(paste ("\\includegraphics < 1 > [height = \\imgsize] {./Rimages/", file, "}", sep = "", collapse = ""), file = lfn)
      catln("\\end {frame}", file = lfn)
      catln("%%\n", file = lfn)
      close (lfn)
      cat("Plot saved to:", file, "\n")
    }
    else {
      ## readline ("savePDF > ")
      cat(file, "not saved, .prn = ", .prn, "\n")
    }
  }
##
savePdf <- savePDF
##
saveWMF <-
  function(file = "Myplot%02d.wmf", comment = "", path = "", height = 6, width = 8, prn = NULL, ...) {
    if (! is.null (prn)) .prn = prn
    if (.prn) {
      ## check if type is present and use .wmf if not
      if (which (substring(paste (file, ".", sep = "", collapse = ""), 1:(nchar (file) + 1), 1:(nchar (file) + 1)) == ".")[1] > nchar (file)) file = paste (file, "wmf", sep = ".")
      win.metafile (paste (path, file, sep = "", collapse = ""), height = height, width = width, ...)
      dev.set(dev.prev())
      dev.copy(which = dev.next())
      dev.off ()
      #
      cat("Plot saved to:", file, "\n")
    }
    else {
      ## readline ("savePDF > ")
      cat(file, "not saved, .prn = ", .prn, "\n")
    }
  }
###
#
my.var <-
  function(..., na.rm = T) {
    n <- length (unlist(...))
    varu <- var (..., na.rm = na.rm) * (n - 1)/n
    return(varu)
  }
#
#
my.sd <-
  function(..., na.rm = T) {
    sdu <- sqrt(my.var (..., na.rm = na.rm))
    return(sdu)
  }
#
peaks <-
  function(x, ...) which (diff (sign(diff (x))) == ( -2))
##


my.latex <-
  function(object, where = "h", file = "", xls = NULL, ctable=TRUE, ...) {
    ## save to xls
    cname=first.word(deparse (substitute (object)))
    if (is.null(xls)) xls <- cname
    lfn <- paste (xls, "xls", sep = ".", collapse = "")
    write.table (object, file = lfn,sep="\t",col.names = NA)
    #cat("Table saved to file: ", lfn,"\n")
    note=paste("File: ","\\url{",lfn,"}")
    latex(object, where = where, file = file,ctable=ctable, insert.bottom=note, ...)
  }
##
pcoord <-
  function(y, scale.data = TRUE, col = "black", lty = "solid", lwd = 1) {
    ## Parallel Coordinates Plot (Ihaka pages)
    ## http://www.stat.auckland.ac.nz/~ihaka/787/software.html
    ## Scale the data (if requested)
    if (scale.data)
      y <- scale (y)
    ## Replicate the line properties
    nobs <- nrow (y)
    col <- rep(col, length = nobs)
    lty <- rep(lty, length = nobs)
    lwd <- rep(lwd, length = nobs)
    ## Produce the plots
    matplot(1:ncol (y), t(y),
            type <- "l",
            col <- col,
            lty <- lty,
            lwd <- lwd, axes = FALSE, ann = FALSE)
    axis(1, at = 1:ncol (y))
    axis(2)
    box()
  }
### from DAAG package
pause <-
  function () {
    cat("Pause. Press < Enter > to continue...")
    readline ()
    invisible ()
  }
purge <-
  function(verbose = FALSE) {
    #
    # detach all datasets
    #
    while (length (grep("package:", search ()[2])) == 0) detach ()
    if (verbose) print(search ())
  }
my.prn <-
  function (x, txt) {
    calltext <- as.character (sys.call ())[2]
    if (! missing(txt)) {
      if (nchar (txt) + nchar (calltext) + 3 > .Options$width)
        calltext <- paste ("\n\n ", calltext, sep = "")
      else txt <- paste (txt, " ", sep = "")
      cat("\n", txt, calltext, "\n\n", sep = "")
    }
    else cat("\n", calltext, "\n\n", sep = "")
    invisible (print(x))
  }
####
####
resetWorkspace <-
  function() {
    ## remove all objects and restore functions from this file
    ##
    if (interactive ()) clr <- winDialog("yesno","Remove all objects?")
    else
      clr <- "YES"
    if (clr=="YES") {
      rm(list=ls(all=TRUE))
      source (file.path (Sys.getenv("R_USER"), "amisc.r"))
    }
  }
#####
## Sweave concordance - included in R 2.8.0
#####
#source("c:/RUSER/Sweave.r")
#print(RweaveLatexFinish)
