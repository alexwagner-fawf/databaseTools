#' Download  data from PostgreSQL-Database
#'
#' This function creates a connection to the Nationalpark Hunsrück-Hochwald
#'  PostgreSQL-database and downloads a specific table with or without a geometry
#'  column by using postgis
#'
#'
#' @param dbSchema character object, name of the database scheme
#' @param db character object, name of the database
#' @param connection pool connection, optional, a pool connection e.g. created with db_con(as_pool = TRUE)
#' @param ... arguments passed to db_con()
#' @return A dataframe with 2 columns - schema name and table names
#' @export

listTables <- function(dbSchema, db = "forschung", connection = NULL, ...){

  if(!"Pool" %in% class(connection)){
    on.exit(DBI::dbDisconnect(connection))
  }

  if(is.null(connection)){
    connection = db_con(...)
  }

  schema_tables <- RPostgreSQL::dbGetQuery(conn = connection, "SELECT table_schema, table_name FROM information_schema.tables")

  schema_tables <- schema_tables[table_schema == dbSchema]

  return(schema_tables)
}
