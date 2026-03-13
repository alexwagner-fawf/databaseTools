#' Upsert Data into a PostgreSQL Table
#'
#' Inserts or updates rows in a PostgreSQL table using an `ON CONFLICT`
#' clause. Data are first uploaded to a temporary staging table and then
#' merged into the target table in a single SQL statement.
#'
#' This function supports standard data frames as well as spatial objects
#' from the \pkg{sf} and \pkg{terra} packages. Connections created by
#' \pkg{DBI}, \pkg{RPostgres}, or \pkg{pool} are supported.
#'
#' @param data A `data.frame`, `sf`, `terra::SpatVector`, or
#'   `terra::SpatRaster` to upload.
#' @param con A database connection object created by
#'   \code{DBI::dbConnect()}, \code{RPostgres::Postgres()}, or a
#'   \code{pool::Pool}.
#' @param schema Character. Target schema name.
#' @param table Character. Target table name.
#' @param id_fields Character vector of column names that define the
#'   unique key used for conflict detection.
#' @param temporary Logical. Use a temporary staging table. Default TRUE.
#' @param analyze Logical. Run `ANALYZE` on the staging table before
#'   merge. Helps performance for very large uploads. Default FALSE.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return Integer. Number of affected rows.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Validates the input data.
#' 2. Writes data to a temporary staging table.
#' 3. Executes a PostgreSQL `INSERT ... ON CONFLICT DO UPDATE`.
#' 4. Drops the staging table automatically.
#'
#' Duplicate key rows in `data` are not allowed and will trigger an error.
#'
#' Geometry columns from \pkg{sf} are preserved automatically when using
#' PostGIS.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "gis")
#'
#' db_upsert(
#'   data = my_df,
#'   con = con,
#'   schema = "core",
#'   table = "projects",
#'   id_fields = "project_id"
#' )
#' }
#'
#' @export
db_upsert <- function(
  data,
  con,
  schema,
  table,
  id_fields,
  temporary = TRUE,
  analyze = FALSE,
  verbose = TRUE
) {

  if (inherits(data, "SpatVector")) {
    data <- sf::st_as_sf(data)
  }

  if (inherits(data, "SpatRaster")) {
    stop("SpatRaster upsert not supported directly. Convert to table first.")
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame, sf, or terra SpatVector.")
  }

  if (!all(id_fields %in% colnames(data))) {
    stop("Some `id_fields` are missing from the data.")
  }

  if (anyDuplicated(data[id_fields])) {
    stop("Duplicate rows detected in `id_fields`. Upsert aborted.")
  }

  if (verbose) {
    message(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " upsert into ", schema, ".", table,
      " (", nrow(data), " rows)"
    )
  }

  target_id <- DBI::Id(schema = schema, table = table)

  tmp_table <- paste0(
    "upsert_",
    table,
    "_",
    as.integer(Sys.time()),
    "_",
    sample(1e6, 1)
  )

  if (temporary) {
    tmp_id <- DBI::Id(schema = "pg_temp", table = tmp_table)
  } else {
    tmp_id <- DBI::Id(schema = schema, table = tmp_table)
  }

  on.exit({
    try(
      DBI::dbExecute(
        con,
        paste("DROP TABLE IF EXISTS", DBI::dbQuoteIdentifier(con, tmp_id))
      ),
      silent = TRUE
    )
  }, add = TRUE)

  if (inherits(data, "sf")) {
    sf::st_write(
      data,
      con,
      layer = tmp_table,
      temporary = temporary,
      append = FALSE,
      quiet = !verbose
    )
  } else {
    DBI::dbWriteTable(
      con,
      name = tmp_id,
      value = data,
      overwrite = TRUE,
      temporary = temporary
    )
  }

  if (analyze) {
    DBI::dbExecute(
      con,
      paste("ANALYZE", DBI::dbQuoteIdentifier(con, tmp_id))
    )
  }

  cols <- colnames(data)

  quoted_cols <- DBI::dbQuoteIdentifier(con, cols)
  quoted_ids <- DBI::dbQuoteIdentifier(con, id_fields)

  non_ids <- setdiff(cols, id_fields)
  quoted_non_ids <- DBI::dbQuoteIdentifier(con, non_ids)

  update_clause <- paste0(
    quoted_non_ids,
    " = EXCLUDED.",
    quoted_non_ids,
    collapse = ", "
  )

  sql <- sprintf(
    paste(
      "INSERT INTO %s (%s)",
      "SELECT %s FROM %s",
      "ON CONFLICT (%s)",
      "DO UPDATE SET %s"
    ),
    DBI::dbQuoteIdentifier(con, target_id),
    paste(quoted_cols, collapse = ", "),
    paste(quoted_cols, collapse = ", "),
    DBI::dbQuoteIdentifier(con, tmp_id),
    paste(quoted_ids, collapse = ", "),
    update_clause
  )

  res <- DBI::dbExecute(con, sql)

  if (verbose) {
    message(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " affected rows: ", res
    )
  }

  invisible(res)
}
