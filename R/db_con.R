

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

