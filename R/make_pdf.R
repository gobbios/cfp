#' compile tex files using biber for bibliographies
#'
#' @param texfile character, file name of tex source file
#' @param clean_up logical, should auxillary files be removed (by default \code{FALSE})
#' @param open_file logical, should the result be opened in the system viewer (by default \code{FALSE}). This probably works on MacOS anyway...
#'
#' @return an invisible list with three entries for latex console output (corresponding to three latex passes) and one item for biber console output
#' @importFrom tools file_path_sans_ext
#' @export
#'

make_pdf <- function(texfile, clean_up = FALSE, open_file = FALSE) {
  # check whether binaries exist and report version numbers
  lat <- Sys.which("pdflatex")
  bib <- Sys.which("biber")
  if (lat == "") stop("pdflatex not found")
  if (bib == "") stop("biber not found")
  system2(bib, "-v")
  system2(lat, "-v")

  # make a copy of any existing pdf for safety reasons...
  fullpath <- normalizePath(texfile)
  purepath <- dirname(fullpath)
  purefile <- file_path_sans_ext(fullpath)
  purefile <- gsub(pattern = purepath, replacement = "", x = purefile)
  purefile <- substr(purefile, 2, nchar(purefile)) # remove initial slash
  x <- file.path(purepath, paste0(purefile, ".pdf"))
  if (file.exists(x)) {
    copyname <- file.path(purepath, paste0("__copy__", purefile, ".pdf"))
    file.copy(from = x, to = copyname)
  }

  # knit tex file
  res <- normalizePath(knitr::knit(fullpath))

  # run latex and biber
  cat("first latex pass\n")
  lat_res1 <- system2(lat, res, stdout = TRUE, stderr = TRUE)
  cat("first biber pass\n")
  bib_res1 <- system2(command = bib,
                      args = normalizePath(file.path(purepath, purefile),
                                           mustWork = FALSE),
                      stdout = TRUE,
                      stderr = TRUE)
  cat("second latex pass\n")
  lat_res2 <- system2(lat, res, stdout = TRUE, stderr = TRUE)
  cat("third latex pass\n")
  lat_res3 <- system2(lat, res, stdout = TRUE, stderr = TRUE)

  # clean up
  if (clean_up) {
    cat("cleaning up\n")
    extensions <- c("bbl", "xdv", "blg", "aux", "bcf", "fls", "log", "out",
                    "txt", "run.xml")
    for (ext in extensions) {
      f <- file.path(purepath, paste0(purefile, ".", ext))
      if (file.exists(f)) {
        x <- file.remove(f)
        if (x) cat("removed:", f, "\n")
        rm(x)
      }
      rm(f)
    }
  }

  if (open_file) {
    system2("open", args = file.path(purepath, paste0(purefile, ".pdf")))
  }

  outres <- list(latex1 = lat_res1,
                 latex2 = lat_res2,
                 latex3 = lat_res3,
                 biber = bib_res1)
  invisible(outres)
}
