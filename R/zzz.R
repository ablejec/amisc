#' Set Package Specific Options when Package is Loaded
#'
#' This function sets package specific options.
#' Option names start with package name and dot (\code{amisc.*}).
#'
#' @param libname
#' a character string giving the library directory where the package defining the namespace was found.
#' @param pkgname
#' a character string giving the name of the package.
#' @return  none (invisible \code{NULL})
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @details \code{.onLoad} sets the \code{amisc.testing} option to \code{FALSE}.
#' This option is used to control testing typeouts in function \code{testing}.
#' @seealso \link{testing}
#' @rdname onLoad
#' @examples
#' (amisc.tst <- getOption("amisc.testing"))
#' options(amisc.testing = TRUE)
#' getOption("amisc.testing")
#' options(amisc.testing = amisc.tst)
#' rm(amisc.tst)
#'
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.amisc <- list(
    amisc.path = "~/R-amisc",
    amisc.install.args = "",
    amisc.name = "Andrej Blejec",
    amisc.desc.author = '"Andrej Blejec <andrej.blejec@nib.si> [aut, cre]"',
    amisc.desc.license = "GPL-3",
    amisc.desc.suggests = NULL,
    amisc.desc = list(),
    amisc.testing = FALSE
  )
  toset <- !(names(op.amisc) %in% names(op))
  if(any(toset)) options(op.amisc[toset])

  invisible()
}
