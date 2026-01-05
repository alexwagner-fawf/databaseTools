#' Test Database Connection
#'
#' Attempts to establish a temporary database connection using credentials
#' stored in environment variables and immediately disconnects if successful.
#'
#' @param driver A DBI-compatible database driver (e.g. \code{RPostgres::Postgres()}).
#' @param port Integer. Database port.
#' @param dbname Character. Name of the database.
#' @param host Character. Database host address.
#'
#' @return Logical. \code{TRUE} if the connection succeeds, otherwise \code{FALSE}.
#'
#' @details
#' This function uses the environment variables \code{DBTools_db_user} and
#' \code{DBTools_db_password} for authentication.
#'
#' @examples
#' \dontrun{
#' connect_test(
#'   driver = RPostgres::Postgres(),
#'   port = 5432,
#'   dbname = "forschung",
#'   host = "10.82.152.222"
#' )
#' }
#'
#' @export

connect_test <- function(driver, port, dbname, host){

  con_working <- try({
    DBI::dbConnect(drv = driver,
                          port = port,
                          dbname = dbname,
                          host = host,
                          user = Sys.getenv("DBTools_db_user"),
                          password = Sys.getenv("DBTools_db_password"))
  })

  con_works <- inherits(con_working, "DBIConnection")

  if(con_works){
    on.exit(DBI::dbDisconnect(con_working))
  }

  if(!con_works){
    warning("issue with database connection - check credentials and run register_environ()")
  }

  return(con_works)
}

#' Register Database Credentials in Environment
#'
#' Stores database username and password in the current R session's
#' environment variables and optionally tests the connection.
#'
#' @param db_user Character. Database username. If \code{NULL}, the user is prompted.
#' @param db_password Character. Database password. If \code{NULL}, the user is prompted securely.
#' @param test_connection Logical. Whether to test the database connection after setting credentials.
#' @param ... Additional arguments passed to \code{\link{connect_test}}.
#'
#' @details
#' Credentials are stored in:
#' \itemize{
#'   \item \code{DBTools_db_user}
#'   \item \code{DBTools_db_password}
#' }
#'
#' If the connection test fails, previous environment variables are restored.
#'
#' @examples
#' \dontrun{
#' register_environ(
#'   db_user = "my_user",
#'   db_password = "my_password",
#'   driver = RPostgres::Postgres(),
#'   dbname = "forschung",
#'   host = "10.82.152.222"
#' )
#'
#' # Interactive usage (prompts for credentials)
#' register_environ(
#'   driver = RPostgres::Postgres(),
#'   dbname = "forschung",
#'   host = "10.82.152.222"
#' )
#' }
#'
#'
register_environ <- function(db_user = NULL,
                             db_password = NULL,
                             test_connection = TRUE,
                             ...){


  prev_db_user <- Sys.getenv("DBTools_db_user")
  prev_db_password <- Sys.getenv("DBTools_db_password")

  while(is.null(db_user) | is.null(db_password)){
    if(is.null(db_user)){
      db_user <- readline("User missing for connect, enter it, to set it in the current environment: ")
    }

    if(is.null(db_password)){
      db_password <- getPass::getPass(noblank = TRUE)
    }
  }


  Sys.setenv("DBTools_db_user" = db_user)
  Sys.setenv("DBTools_db_password" = db_password)

  if(test_connection){
    if(!connect_test(...)){
      Sys.setenv("DBTools_db_user" = prev_db_user)
      Sys.setenv("DBTools_db_password" = prev_db_password)
    }
  }
}

#' Create Database Connection or Connection Pool
#'
#' Establishes a database connection using stored environment credentials.
#' Automatically prompts for credentials if missing or invalid.
#'
#' @param driver A DBI-compatible database driver.
#' @param port Integer. Database port.
#' @param dbname Character. Name of the database.
#' @param host Character. Database host address.
#' @param as_pool Logical. If \code{TRUE}, returns a connection pool via \pkg{pool}.
#'
#' @return
#' A \code{DBIConnection} object or a \code{pool} object.
#'
#' @details
#' The function retries up to four times to establish a connection, prompting
#' the user for credentials if needed.
#'
#' @examples
#' \dontrun{
#' # Standard DBI connection
#' con <- db_con()
#'
#' # Using a connection pool
#' pool <- db_con(as_pool = TRUE)
#'
#' # Custom connection parameters
#' con <- db_con(
#'   driver = RPostgres::Postgres(),
#'   dbname = "forschung",
#'   host = "10.82.152.222"
#' )
#' }
#'
#' @export
db_con <- function(driver = RPostgres::Postgres(),
                   port = 5432,
                   dbname = "forschung",
                   host = "10.82.152.222",
                   as_pool = FALSE){

  try = 0
  while(!suppressWarnings(connect_test(driver = driver, port = port, dbname = dbname, host = host)) & !try >= 4){
    register_environ(test_connection = FALSE)
    try = try + 1
    print(try)
  }

  if(try == 4){
    stop("Error when trying to establish connection.
         Check credentials and default values in databaseTools::db_con.
         Run register_environ() to set username and password. Is the Server running?")
  }

  con <- tryCatch({
    if(as_pool){
      con <- pool::dbPool(drv = driver,
                          port = port,
                          dbname = dbname,
                          host = host,
                          user = Sys.getenv("DBTools_db_user"),
                          password = Sys.getenv("DBTools_db_password"))
    }else{
      con <- DBI::dbConnect(drv = driver,
                            port = port,
                            dbname = dbname,
                            host = host,
                            user = Sys.getenv("DBTools_db_user"),
                            password = Sys.getenv("DBTools_db_password"))
    }

    return(con)

  },
  error = function(e){
    warning("Error when trying to establish connection.
         Check credentials and default values in databaseTools::db_con.
         Run register_environ() to set username and password.")
  })

  return(con)

}

