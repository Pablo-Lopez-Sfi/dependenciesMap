


.onAttach <- function(libname, pkgname){

  relations <<- data.frame( from = character(0), to = character(0), time = character(0) )
  inputs_file <<- c()
  inputs_db <<- c()
  inputs_df <<- c()
  outputs_file <<- c()
  outputs_db <<- c()
  outputs_df <<- c()

}


