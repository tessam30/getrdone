.onAttach <- function(libname, pkgname) {
  message <- c("\n Thank you for using the {getrdone} R package!",
               "\n \n This package is designed for working with Zambia data and generating Zambia visuals.",
               "\n If you encounter any bugs, please email tessam@usaid.gov to report any issues.",
               "\n \n Happy coding!")
  packageStartupMessage(message)
}