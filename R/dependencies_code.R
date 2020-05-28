



#' dependenciesMap: A package that allows to create objects in order to map dependencies in scripts
#'
#' Each object of this package is used to map an input or an output that can be :\itemize{
#' \item A file (RData, csv, xlsx) : \code{\link{Input_file}} and \code{\link{Output_file}}
#' \item A db table : \code{\link{Input_db}} and \code{\link{Output_db}}
#' \item A dataframe : \code{\link{Input_df}} and \code{\link{Output_df}}
#' }
#'
#' @docType package
#' @name dependenciesMap
NULL




#' Initializes objects that will record inputs, outputs and dependencies
#'
init_dependencies <- function(){

  relations <<- data.frame( from = character(0), to = character(0), time = character(0) )
  inputs_file <<- c()
  inputs_db <<- c()
  inputs_df <<- c()
  outputs_file <<- c()
  outputs_db <<- c()
  outputs_df <<- c()

}




#' Inputs loaded in the data preparation program from files
#'
#' @field file : name of the file loaded (R.data, csv, xlsx)
#' @field path : path of file (NULL if omitted)
#' @field version : version in path ( DEV/ for Development, blank for production) (NULL if omitted)
#' @field process : process where input is loaded in the data preparation program (to be added by user)
#'
Input_file <- setRefClass("Input_file",

                          fields = list(file = "character",
                                        path = "ANY",
                                        version = "ANY",
                                        process = "character",
                                        date = "ANY"),

                          methods = list(

                            initialize = function(..., path = NULL, version = NULL, date = Sys.time()){
                              callSuper(..., path = path, version = version, date = date)
                            },

                            insert_relation = function() {
                              assign("relations",
                                     rbind(relations,
                                           list( "from" = file, "to" = process, "time" = as.character(date) ),
                                           stringsAsFactors = F),
                                     envir = .GlobalEnv)
                              assign("inputs_file", c(inputs_file , .self),  envir = .GlobalEnv)
                            },

                            f_load = function(...){
                              "function \\code{load()} \n
                               By defaut : \\code{load( paste0( path, version, file ), envir = .GlobalEnv, ...)} \n
                               \\subsection{Example}{\\code{load( 'SalesnFcst.Rdata' )} is now replaced by\n
                               \\code{Input_file( file = 'SalesnFcst.Rdata', process = 'SalesnFcst' )$f_load()}  (version and path = NULL)}"
                              insert_relation()
                              load( file = paste0(path, version, file), envir = .GlobalEnv, ...)
                            },

                            f_read.csv = function(...){
                              "function \\code{read.csv()} \n
                              By default : \\code{read.csv( paste0( path, version, file ), ... )} \n
                              \\subsection{Example}{\\code{read.csv( 'Dependencies_Map/dependencies.csv' )} is now replaced by\n
                              \\code{Input_file( file = 'dependencies.csv', path = 'Dependencies_Map/', process = 'SalesnFcst')$f_read.csv()}  (version = NULL) }"
                              insert_relation()
                              read.csv(paste0(path, version, file),...)
                            },

                            f_read.xlsx = function(...){
                              "function \\code{read.xlsx()} from openxlsx package\n
                               By default : \\code{openxlsx::read.xlsx( paste0( path, version, file ), ... )} \n
                               \\subsection{Example}{\\code{read.xlsx( paste0( PathLoad, Version, 'Baselines_and_forecastable.xlsx' ), sheet = 'SkuMatrix', colNames = TRUE  )} is now replaced by\n
                               \\code{Input_file( file = 'Baselines_and_forecastable.xlsx', path = PathLoad, version = Version, process = 'SalesnFcst' )$f_read.xlsx( sheet = 'SkuMatrix', colNames = TRUE )} }"
                              insert_relation()
                              openxlsx::read.xlsx(paste0(path, version, file),...)
                            },

                            f_read.xls = function(...){
                              "function \\code{read.xls()} from gdata package\n
                               By default : \\code{gdata::read.xls( paste0( path, version, file ), ... )}"
                              insert_relation()
                              gdata::read.xls(paste0(path, version, file),...)
                            }

                          )
)


#' Inputs loaded in the data preparation program from data base's tables
#'
#' @field conn : connection of the data base
#' @field query : query
#' @field process : process where input is loaded in the data preparation program (to be added by user)
#' @field table : name of the data base's table used to get the data (to be added by the user)
#'
Input_db <- setRefClass("Input_db",

                        fields = list(conn = "ANY",
                                      query = "character",
                                      process = "character",
                                      table = "character",
                                      date = "ANY"),

                        methods = list(

                          initialize = function(..., date = Sys.time()){
                            callSuper(..., date = date)
                          },

                          insert_relation = function() {
                            assign("relations",
                                   rbind(relations,
                                         list( "from" = paste0("[", tolower(table), "]" ), "to" = process, "time" = as.character(date) ),
                                         stringsAsFactors = F),
                                   envir = .GlobalEnv)
                            assign("inputs_db", c(inputs_db , .self),  envir = .GlobalEnv)
                          },

                          f_dbGetQuery = function(){
                            "function \\code{dbGetQuery()} from DBI package\n
                             By default : \\code{dbGetQuery( conn, query )}\n
                             \\subsection{Example}{ \\code{par_loc <- dbGetQuery( SCDatabase, 'select * from PAR_LOC_geoMapping' ) } is now replaced by\n
                            \\code{par_loc <- Input_db( conn = SCDataBase, query = 'select * from PAR_LOC_geoMapping', process = 'SLA', table = 'PAR_LOC_geoMapping')$f_dbGetQuery()  } }"
                            insert_relation()
                            dbGetQuery(conn, query)
                          }

                        )
)



#' Inputs loaded in the data preparation program from data frames previously generated
#'
#' @field name : name of the data frame loaded (to be added by user)
#' @field data : data loaded (optional attribute)
#' @field process : process where input is loaded in the data preparation program (to be added by user)
#'
Input_df <- setRefClass("Input_df",

                               fields = list(name = "character",
                                             data = "ANY",
                                             process = "character",
                                             date = "ANY"),

                               methods = list(

                                 initialize = function(..., data = NULL, date = Sys.time()){
                                   callSuper(..., data = data, date = date)
                                 },

                                 insert_relation = function() {
                                   assign("relations",
                                          rbind(relations,
                                                list( "from" = name, "to" = process, "time" = as.character(date) ),
                                                stringsAsFactors = F),
                                          envir = .GlobalEnv)
                                   assign("inputs_df", c(inputs_df , .self),  envir = .GlobalEnv)
                                 },

                                 f_return = function(...){
                                   "simply returns the data of the object \n
                                   By default : \\code{return(data)}\n
                                   \\subsection{Example}{\\code{MasterData2 <- MasterData}  is now replaced by\n
                                   \\code{MasterData2 <- Input_df( name = 'MasterData', data = MasterData, process = 'Future_Forecast' )$f_return()} }"
                                   insert_relation()
                                   return(data)
                                 },

                                 f_add = function(){
                                   "does nothing, just allows to keep track of the new data frame used as input\n
                                    to be used when in a process, a new data frame is used and there is no need to return it\n
                                    \\subsection{Example}{ \\code{Input_df( name = 'MasterData', data = MasterData, process = 'Future_Forecast' )$f_add()}}"
                                   insert_relation()
                                 }


                               )
)



#' Output files saved in the data preparation program
#'
#' @field file : name of the file that is going to be saved (R.data, csv, xlsx)
#' @field data : data that is going to be saved
#' @field data_name : name of the variable that contains the data (string)
#' @field path : path of file (NULL if omitted)
#' @field version : version in path ('DEV/' for Development, '' for production) (NULL if omitted)
#' @field process : process where input is loaded in the Data preparation program (to be added by user)
#'
Output_file <- setRefClass("Output_file",

                           fields = list(file = "character",
                                         data = "ANY",
                                         path = "ANY",
                                         version = "ANY",
                                         process = "character",
                                         data_name = "character",
                                         date = "ANY"),

                           methods = list(

                             initialize = function( ..., path = NULL, version = NULL, date = Sys.time() ){
                               callSuper(..., path = path, version = version, date = date)
                             },

                             insert_relation = function() {
                               assign("relations",
                                      rbind( relations,
                                             list( "from" = process, "to" = file, "time" = as.character(date) ),
                                             stringsAsFactors = F ),
                                      envir = .GlobalEnv)
                               assign("outputs_file", c(outputs_file, .self), envir = .GlobalEnv)
                             },

                             f_save = function(...){
                              "function \\code{save()} \n
                              By default : \\code{save( data, paste0(path, version, file), ... )}\n
                               \\subsection{Example}{\\code{save( MasterData, file =  paste0( PathLoad, Version, 'SalesnFcst.Rdata' ) )} is now replaced by\n
                               \\code{Output_file(data = MasterData, data_name = 'MasterData', file = 'SalesnFcst.Rdata', path = PathLoad, version = Version, process = 'SalesnFcst')$f_save()} }"
                               insert_relation()
                               assign(data_name,data)
                               save( list = data_name , file = paste0(path, version, file),...)
                             }

                           )
)



#' Outputs saved in the data preparation program in data base's tables
#'
#' @field conn : connection of the data base
#' @field data : data that is going to be saved in the data base
#' @field query : query
#' @field FUN_DB : fun used in function DBConnectWrite (MyConSCDIRwrite)
#' @field lastmonths : parameter from function MyAppend
#' @field datecolumn : parameter from function Myappend
#' @field FUN_DB : fun used in function DBConnectWrite (MyConSCDIRwrite)
#' @field table : name of the data base's table used to save the data (to be added by the user)
#'
Output_db <- setRefClass("Output_db",

                         fields = list(conn = "ANY",
                                       query = "ANY",
                                       data = "ANY",
                                       table = "ANY",
                                       FUN_DB = "ANY",
                                       datecolumn = 'ANY',
                                       lastmonths = 'ANY',
                                       process = "character",
                                       date = "ANY"),

                         methods = list(

                           initialize = function( ..., query = NULL, FUN_DB = NULL, datecolumn = NULL, lastmonths = NULL, date = Sys.time() ){
                             callSuper(..., query = query, FUN_DB = FUN_DB, datecolumn = datecolumn, lastmonths = lastmonths, date = date)
                           },

                           insert_relation = function() {
                             assign("relations",
                                    rbind(relations,
                                          list( "from" = process, "to" = paste0("[", tolower(table), "]" ), "time" = as.character(date) ),
                                          stringsAsFactors = F ),
                                    envir = .GlobalEnv)
                             assign("outputs_db", c(outputs_db, .self), envir = .GlobalEnv)
                           },

                           f_DBConnectWrite = function(...){
                             "function \\code{DBConnectWrite()} from MyFunnew.R script in data preparation program (a source must be done beforehand to MyFun.R script)\n
                             By defaut : \\code{DBConnectWrite( conn, data, table, FUN_DB )}\n
                             \\subsection{Example}{\\code{LogWrite <- DBConnectWrite( SCDataBase, QVlocs, 'PAR_LOC_geoMapping', MyConSCDIRwrite )} is now replaced by\n
                             \\code{LogWrite <- Output_db( conn = SCDatabase, data = QVlocs, table = 'PAR_LOC_geoMapping', FUN_DB = MyConSCDIRWrite, process = 'SLA' )$f_DBConnectWrite()} } "
                             insert_relation()
                             DBConnectWrite(conn, data, table, FUN_DB, ...)
                           },

                           f_Myappend = function(){
                             "function \\code{Myappend()} from MyFunnew.R script in data preparation program (a source must be done beforehand to MyFun.R script)\n
                             By defaut : \\code{Myappend(conn, data, table, datecolumn, lastmonths)}\n
                             \\subsection{Example}{\\code{LogWrite <- Myappend(SCDataBase, ISL, 'cal_isl', 'DATE' , (pstmnths2run+1) )} is now replaced by\n
                             \\code{LogWrite <- Output_db( conn = SCDataBase, data = ISL, table = 'cal_isl', datecolumn = 'DATE', lastmonths = (pstmnths2run+1), process = 'ISL' )$f_Myappend()} } "
                             insert_relation()
                             Myappend(conn, data, table, datecolumn, lastmonths)
                           },

                           f_dbSendUpdate = function(...){
                             "function \\code{dbSendQuery()} from DBI package.\n
                             By defaut : \\code{dbSendUpdate( conn, query, ...)} \n
                             \\subsection{Example}{\\code{dbSendUpdate( SCDataBase, 'INSERT INTO sla_history SELECT * FROM slatemp' )} is now replaced by\n
                             \\code{Output_db( conn = SCDatabase, query = 'INSERT INTO sla_history SELECT * FROM slatemp', table = 'sla_history', process = 'SLA' )$dbSendUpdate()} }"
                             insert_relation()
                             dbSendUpdate(conn, query,...)
                           },

                           f_dbWriteTable = function(...){
                             "function \\code{dbWriteTable()} from DBI package.\n
                              By defaut : \\code{dbWriteTable( conn, table, data, ... )}\n
                              \\subsection{Example}{\\code{dbWriteTable(SCDataBase, 'PAR_LOC_geoMapping', QVlocs, row.names = FALSE, append = FALSE,overwrite = TRUE, date = TRUE, ora.number = ORANUMBER)} is now replaced by \n
                             \\code{Output_db( conn = SCDataBase, table = 'PAR_LOC_geoMapping', data = QVlocs, process = 'Total_Supply' )$f_dbWriteTable( row.names = FALSE, append = FALSE,overwrite = TRUE, date = TRUE, ora.number = ORANUMBER )} } "
                             insert_relation()
                             dbWriteTable(conn, table, data, ...)
                           }

                         )
)



#' Outputs in the data preparation program from data frames previously generated (can be used to modified a global variable)
#'
#' @field name : name of the data frame (to be added by user)
#' @field data : data  generated (NULL if omitted)
#' @field process : process where output is generated in the data preparation program (to be added by user)
#'
Output_df <- setRefClass("Output_df",

                                fields = list(name = "character",
                                              data = "ANY",
                                              process = "character",
                                              date = "ANY"),

                                methods = list(

                                  initialize = function(..., data = NULL, date = Sys.time()){
                                    callSuper(..., data = data, date = date)
                                  },

                                  insert_relation = function() {
                                    assign("relations",
                                           rbind(relations,
                                                 list( "from" = process, "to" = name, "time" = as.character(date) ),
                                                 stringsAsFactors = F),
                                           envir = .GlobalEnv)
                                    assign("outputs_df", c(outputs_df , .self),  envir = .GlobalEnv)
                                  },

                                  f_return = function(...){
                                    "simply returns the data of the object :\n
                                     By default: \\code{return(data)}\n
                                     \\subsection{Example}{\\code{MasterData <- left_join( MasterData, Adj_CoV_Result, by = c('DMDUNIT','LOC','DMDGROUP','STARTDATE') ) } is now replace by\n
                                    ( \\code{new_data <- left_join( MasterData, Adj_CoV_Result, by = c('DMDUNIT','LOC','DMDGROUP','STARTDATE'))} )\n
                                    \\code{MasterData <- Output_df( name = 'MasterData', data = new_data, process = 'CoV' )$f_return()}}"
                                    insert_relation()
                                    return(data)
                                  },

                                  f_add = function(){
                                    "does nothing, just allows to keep track of the new data frame created as output\n
                                    to be used when in a process, a new data frame is created and there is no need to return it\n
                                    \\subsection{Example}{\\code{Output_df( name = 'MasterData', data = new_data, process = 'CoV' )$f_add()}}
                                    "
                                    insert_relation()
                                  }


                                )
)



#' Gets the names of the processes used in the input and output objects
#'
getProcesses <- function() {

  processes <- c()

  objects <- c("inputs_file", "inputs_db", "inputs_df",
               "outputs_file", "outputs_db", "outputs_df" )

  for( i in objects ){
    for( obj in get(i) ){
      processes <- c(processes, obj$process)
    }
  }

  return(unique(processes))

}




