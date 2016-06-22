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
#' @param ... R objects (see \link{cat} 'Details' for the types
#' of objects allowed).
#' @param file see \link{read.table}.
#' @param sep see \link{read.table}.
#' @param fill see \link{read.table}.
#' @param labels  see \link{read.table}.
#' @param append see \link{read.table}.
#' @param EOL Object to append (default is end-of-line \code{\\n}).
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
#' @param prefix character string to be printed before values.
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
# ------------------------------------------------------------
#' MLE Variance Estimator
#'
#' Compute variance using the MLE variance estimator.
#'
#' @param x numeric vector
#' @param na.rm logical. Should missing values be remove?
#' @return MLE estimate of variance. Note that it is biased.
#' @export
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' Var(1:4)
#' 3/4*var(1:4)
Var <-
  function(x, na.rm=FALSE) {
    #
    # MLE variance estimator
    #
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)
    return(sum((x-mean(x))^2)/n)
  }

