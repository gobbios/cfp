#' compile tex files using biber for bibliographies
#'
#' @param texfile character, file name of tex source file
#' @param bibfolder character, folder location of bib files
#' @param figfolder character, folder location of figures for inclusion
#' @param clean_up logical, should auxillary files be removed (by default \code{FALSE})
#' @param open_file logical, should the result be opened in the system viewer (by default \code{FALSE}). This probably works on MacOS anyway...
#' @param drytest logical, stop before compilation and return temp directory (default is \code{FALSE})
#'
#' @details Note that the locations of tex files and folders must all start in the same place. For example, if I have my tex files in \code{documents/folder1/} and the corresponding figures in \code{documents/folder1/figures/}, while the references are in \code{documents/reffolder/} I need to start all locations from \code{documents}: \code{texfile = "folder1/mytex.tex"}, \code{figfolder = "folder1/figures/"} and \code{bibfolder = "reffolder/"}. This is all pretty convoluted, I realize, but it's due to the structure of my folder system.
#'
#' In addition, please notice that if there is something wrong with folder/file structure, R might crash (which is most likely due to \code{pdflatex} crashing when it doesn't find referenced files. It is therefore recommended to inspect the temp folder created by setting \code{drytest = TRUE}: this returns the location of the temp folder. You should check the \emph{relative} location of the different files is correct.
#'
#' @return an invisible list with three entries for latex console output (corresponding to three latex passes) and one item for biber console output
#' @importFrom tools file_path_sans_ext
#' @export
#'

make_pdf <- function(texfile, bibfolder, figfolder, clean_up = FALSE, open_file = FALSE, drytest = FALSE) {
  # check whether binaries exist and report version numbers
  lat <- Sys.which("pdflatex")
  bib <- Sys.which("biber")
  if (lat == "") stop("pdflatex not found")
  if (bib == "") stop("biber not found")
  system2(bib, "-v")
  system2(lat, "-v")

  # make a copy of an existing pdf for safety reasons...
  fullpath <- normalizePath(texfile)
  purepath <- dirname(fullpath)
  purefile <- tools::file_path_sans_ext(fullpath)
  purefile <- gsub(pattern = purepath, replacement = "", x = purefile)
  purefile <- substr(purefile, 2, nchar(purefile)) # remove initial slash
  x <- file.path(purepath, paste0(purefile, ".pdf"))
  if (file.exists(x)) {
    copyname <- file.path(purepath, paste0("__copy__", purefile, ".pdf"))
    file.copy(from = x, to = copyname, overwrite = TRUE)
    file.remove(x)
  }

  # set up temporary locations for files use temp dir
  tdir <- normalizePath(file.path(tempdir(), "textemp"), mustWork = FALSE)
  tdir <- gsub("\\/\\/", "\\/", tdir)
  if (!dir.exists(tdir)) dir.create(tdir)

  # create folder for figure-includes
  if (!dir.exists(file.path(tdir, figfolder))) {
    dir.create(file.path(tdir, figfolder), recursive = TRUE)
  }
  # copy files from fig folder to temp location
  figs <- list.files(figfolder)
  file.copy(from = list.files(figfolder, full.names = TRUE),
            to = file.path(tdir, figfolder, figs))

  # create folder for bibs
  if (!dir.exists(file.path(tdir, bibfolder))) {
    dir.create(file.path(tdir, bibfolder), recursive = TRUE)
  }

  # copy bib files
  bibs <- list.files(bibfolder)
  file.copy(from = list.files(bibfolder, full.names = TRUE),
            to = file.path(tdir, bibfolder, bibs))

  # copy tex file
  file.copy(from = texfile, to = file.path(tdir, texfile))

  if (drytest) {
    return(tdir)
  }

  # current working dir
  W <- getwd()

  # set working directory to location of texfile
  texfiledir <- dirname(file.path(tdir, texfile))
  setwd(texfiledir)
  xres <- paste0(purefile, ".txt")
  knitr::knit(basename(texfile))

  # run latex and biber
  cat("first latex pass\n")
  lat_res1 <- system2(lat, xres, stdout = TRUE, stderr = TRUE)
  cat("first biber pass\n")
  bib_res1 <- system2(command = bib,
                      args = purefile,
                      stdout = TRUE,
                      stderr = TRUE)
  cat("second latex pass\n")
  lat_res2 <- system2(lat, xres, stdout = TRUE, stderr = TRUE)
  cat("third latex pass\n")
  lat_res3 <- system2(lat, xres, stdout = TRUE, stderr = TRUE)

  # set wd back to original
  setwd(W)


  respath <- file.path(dirname(file.path(tdir, texfile)), paste0(purefile,
                                                                 ".pdf"))
  outloc <- file.path(purepath, paste0(purefile, ".pdf"))
  if (file.exists(respath)) {
    file.copy(from = respath, to = outloc)
    if (file.exists(outloc)) cat("output written to", outloc, "\n")
  }


  # clean up
  if (clean_up) {
    cat("cleaning up\n")
    file.remove(list.files(tdir, full.names = TRUE, recursive = TRUE))
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
