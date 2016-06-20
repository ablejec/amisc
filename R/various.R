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
# ------------------------------------------------------------
#' Concatenate and Print with End-Of-Line
#'
#' Outputs the objects and EOF, concatenating the representations.
#'
#' @param ... R objects (see \link{cat} 'Details' for the types of objects allowed).
#' @param EOL Object to append (default is end-of-line \code{\\n})
#' @return None (invisible NULL).
#' @note
#' This function is mostly useful for printing values to console in plain R.
#' RStudio always ends the line for \code{cat}.
#' @export
#' @seealso \link{cat}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' catln("Two numbers:",1,2)
#' # Ended by tab and dot
#' catln("Number:",1,EOL="\t.")
catln <-
  function(..., file = "", sep = " ", fill = FALSE, labels = NULL,
           append = FALSE,EOL="\n") {
    cat(..., EOL, file=file, sep=sep, fill=fill, labels=labels,append=append)
  }
# ------------------------------------------------------------
#' Print Object Names and Values
#'
#' Outputs the objects names and values.
#'
#' @param ... R objects.
#' @return None (invisible NULL).
#' @details
#' This function is useful for debugging where it can be useful to know
#' the name of the object associated with the displayed value.
#'
#' The actual printout depends on option \code{getOption("amisc.testing")}.
#' If it is set to \code{FALSE} (default) nothing happens.
#' If set to  \code{options(amisc.testing = TRUE)} test output is produced.
#' @export
#' @seealso To control options see \link{options} and \link{.onLoad}, \link{cat} for print controls.
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' .oldOpt <- options(amisc.testing = TRUE)
#' x <- 3.14
#' testing(x)
#' y <- 1:5
#' testing(y, prefix="-------------> \n...")
#' options(amisc.testing = FALSE)
#' testing(y)
#' options(.oldOpt) # reset option
#' rm(.oldOpt)
testing <-
  function(...,prefix=" Testing > > > \n" ) if (getOption("amisc.testing")) cat(prefix, deparse (substitute (...)), " = ", ...,"\n")
#
options(keep.source = TRUE)
# ------------------------------------------------------------
#' Read Clipboard
#'
#' Read text (usually table with data) from clipboard. This is a wrapper function for \code{read.table} with different defaults.
#'
#' @param header
#' a logical value indicating whether the file contains the names of the variables as its first line. Defaults to \code{TRUE}. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
#' @param sep
#' the field separator character (default \code{\\t}). Values on each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.
#' @return A data frame. See \link{read.table}
#' @details
#' This function is useful for debugging where it can be useful to know
#' the name of the object associated with the displayed value.
#'
#' The actual printout depends on option \code{getOption("amisc.testing")}.
#' If it is set to \code{FALSE} (default) nothing happens.
#' If set to  \code{options(amisc.testing = TRUE)} test output is produced.
#' @export
#' @seealso \link{read.table}.
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' # Copy some data to clipboard before running the code below
#' read.clipboard()
read.clipboard <-
  function (header = T, sep = "\t", ...) {
    read.table (file = "clipboard", header = header, sep = sep, ...)
  }
# ------------------------------------------------------------
#' Pause Execution
#'
#' Stops execution and waits for any key to be pressed.
#'
#' @param prompt the string printer when prompting the user to press a key.
#' @return NULL
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples \dontrun{
#' for (i in 1:3) {
#'   print(i)
#'   pause()
#' }
#' }
pause <-
  function (prompt="Pause. Press <Enter> to continue... ") {
    if(interactive()) readline (prompt)
    invisible ()
  }
# ------------------------------------------------------------
#' Detach all Attached Datasets
#'
#' All attached datasets will be detached. Especially useful
#' after a number of attaces of the same dataset
#' (in interactive mode it happens).
#'
#' @param verbose print final search path list for confirmaton
#' @return NULL
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' bla <- data.frame(x=1,y=1)
#' try(print(x))
#' attach(bla)
#' attach(bla)
#' search()
#' purge()
#' search()
#' #
#' try(x)
#' attach(bla)
#' try(x)
#' purge(verbose=TRUE)
#' #
#' rm(bla)
purge <-
  function(verbose = FALSE) {
    #
    # detach all datasets
    #
    while (length (grep("package:", search ()[2])) == 0) detach ()
    if (verbose) print(search ())
  }

