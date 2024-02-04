#' @importFrom utils citation
#'
.onAttach<-function(libname, pkgname){
  requireNamespace("utils")
  cite1<-citation(pkgname)
  cite1<-paste(c(format(cite1,"citation")),collapse="\n\n")
  packageStartupMessage(paste0("
  Please refer to GitHub to post bug reports: https://github.com/falkcarl/multilevelmediation\n
  Software development and maintenance requires much effort.
  If you use this package in a publication, please consider a citation.
                               \n",
                               cite1
  ))
}
0
