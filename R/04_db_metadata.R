LL_SCHEMAS <- c("ros_meta", "ros_common", "ros_ll")
PS_SCHEMAS <- c("ros_meta", "ros_common", "ros_ps")

db_metadata_table <- R6Class(
  "DbMetadataTable",
  public = list(
    initialize = function(schema, table, comment, columns) {
      stopifnot(!is.na(schema), is.character(schema), nchar(schema) > 0)
      stopifnot(!is.na(table), is.character(table), nchar(table) > 0)
      private$.schema <- schema
      private$.table <- table
      private$.comment <- ifelse(str_length(comment) == 0, NA, comment)
      private$.columns <- columns
    },
    table_comment = function() {
      private$.comment
    },
    schema = function() {
      private$.schema
    },
    table = function() {
      private$.table
    },
    columns = function() {
      private$.columns
    },
    table_columns = function() {
      data.table(self$columns())[, schema := NULL][, table := NULL]
    },
    column_names = function() {
      private$.columns$column_name
    }
  ),
  private = list(
    # schema
    .schema = NULL,
    # table
    .table = NULL,
    # comment
    .comment = NULL,
    # columns
    .columns = NULL
  )
)

db_metadata_schema <- R6Class(
  "DbMetadataSchema",
  public = list(
    initialize = function(schema, comment, tables) {
      stopifnot(!is.na(schema), is.character(schema), nchar(schema) > 0)
      private$.schema <- schema
      private$.comment <- ifelse(str_length(comment) == 0, NA, comment)
      private$.tables <- tables
    },
    schema = function() {
      private$.schema
    },
    schema_comment = function() {
      private$.comment
    },
    table_names = function() {
      names(private$.tables)
    },
    tables = function(schema) {
      Filter(function(x) { x$schema() == schema }, private$.tables)
    },
    all_tables = function() {
      private$.tables
    },
    table = function(table) {
      unlist(Filter(function(x) { x$table() == table }, private$.tables))[[1]]
    },
    to_table_comments = function() {
      tables <- self$all_tables()
      temp <- lapply(tables, function(x) { data.table(schema = x$schema(), table = x$table(), comment = ifelse(is.null(x$table_comment()), NA, x$table_comment())) })
      data <- rbindlist(temp)
      data
    },
    to_table_columns = function() {
      data <- rbindlist(lapply(self$all_tables(), function(x) { x$columns() }))
    }
  ),
  private = list(
    # schema
    .schema = NULL,
    # comment
    .comment = NULL,
    # tables
    .tables = NULL
  )
)

db_metadata <- R6Class(
  "DbMetadata",
  public = list(
    initialize = function(domain, version, schemas_comment, tables_comment, tables_columns) {
      stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
      private$.domain <- domain
      private$.version <- version
      schema_names <- schemas_comment$schema
      private$.schemas <- lapply(schema_names, function(schema_name) {
        comment <- schemas_comment[schema == schema_name]$comment
        table_comment <- tables_comment[schema == schema_name]
        table_names <- table_comment$table
        columns <- tables_columns[schema == schema_name]
        columns <- lapply(table_names, function(table_name) {
          db_metadata_table$new(schema_name, table_name, table_comment[table == table_name]$comment, columns[table == table_name])
        })
        names(columns) <- table_names
        db_metadata_schema$new(schema_name, comment, columns)
      })
      names(private$.schemas) <- schema_names
    },
    domain = function() {
      private$.domain
    },
    version = function() {
      private$.version
    },
    all_schemas = function() {
      private$.schemas
    },
    schema = function(schema_name) {
      Filter(function(x) { x$schema() == schema_name }, private$.schemas)[[1]]
    },
    schema_names = function() {
      names(private$.schemas)
    },
    to_schema_comments = function() {
      schemas <- self$all_schemas()
      data <- lapply(schemas, function(x) { ifelse(is.null(x$schema_comment()), NA, x$schema_comment()) })
      data <- data.table(names(data), data)
      names(data) <- c("schema", "comment")
      data
    },
    to_table_comments = function() {
      rbindlist(lapply(self$all_schemas(), function(x) { x$to_table_comments() }))
    },
    to_table_columns = function() {
      rbindlist(lapply(self$all_schemas(), function(x) { x$to_table_columns() }))
    }
  ),
  private = list(
    # domain
    .domain = NULL,
    # version
    .version = NULL,
    # schemas
    .schemas = NULL
  )
)

get_schemas_comment <- function(schema_names, connection_provider = connect_to_ros) {
  use_connection(connection_provider, function(connection) {
    sql <- "
SELECT
    n.nspname AS schema,
    d.description AS comment
FROM pg_namespace n
LEFT JOIN pg_description d
       ON d.objoid = n.oid
      AND d.classoid = 'pg_namespace'::regclass
      AND d.objsubid = 0
WHERE n.nspname NOT IN ('pg_catalog', 'information_schema') AND n.nspname IN ($1)
ORDER BY n.nspname
"
    query(connection, sql, params = list(schema_names))
  })
}

get_tables_comment <- function(schema_names, connection_provider = connect_to_ros) {
  use_connection(connection_provider, function(connection) {
    sql <- "
SELECT
    t.table_schema AS schema,
     t.table_name AS table,
    obj_description(c.oid, 'pg_class') AS comment
FROM information_schema.tables t
JOIN pg_class c ON c.relname = t.table_name
JOIN pg_namespace n ON n.oid = c.relnamespace AND n.nspname = t.table_schema
WHERE t.table_schema IN ($1)
  AND t.table_type = 'BASE TABLE'
ORDER BY t.table_name"
    query(connection, sql, params = list(schema_names))
  })
}

get_tables_columns <- function(schema_names, connection_provider = connect_to_ros) {
  use_connection(connection_provider, function(connection) {
    sql <- "
SELECT
    cols.table_schema AS schema,
    cols.table_name AS table,
    cols.column_name AS column,
    format_type(a.atttypid, a.atttypmod) AS type,
    CASE
        WHEN (NOT cols.is_nullable::boolean) THEN 'YES'
        ELSE 'NO'
    END AS mandatory,
        pgd.description AS comment,
--    fk.constraint_name,
    CASE
        WHEN fk.target_schema IS NULL THEN NULL
        ELSE concat(fk.target_schema, '.', fk.target_table, '→', fk.target_column)
    END AS foreign_key
--    fk.target_schema,
--    fk.target_table,
--    fk.target_column
FROM information_schema.columns cols

JOIN pg_namespace ns
    ON ns.nspname = cols.table_schema

JOIN pg_class tbl
    ON tbl.relname = cols.table_name
   AND tbl.relnamespace = ns.oid

JOIN pg_attribute a
    ON a.attrelid = tbl.oid
   AND a.attname = cols.column_name

LEFT JOIN pg_description pgd
    ON pgd.objoid = tbl.oid
   AND pgd.objsubid = a.attnum
LEFT JOIN (
    SELECT
        con.conname AS constraint_name,
        src_ns.nspname AS source_schema,
        src_tbl.relname AS source_table,
        src_col.attname AS source_column,
        target_ns.nspname AS target_schema,
        target_tbl.relname AS target_table,
        target_col.attname AS target_column

    FROM pg_constraint con
    JOIN pg_class src_tbl
        ON src_tbl.oid = con.conrelid
    JOIN pg_namespace src_ns
        ON src_ns.oid = src_tbl.relnamespace
    JOIN pg_class target_tbl
        ON target_tbl.oid = con.confrelid
    JOIN pg_namespace target_ns
        ON target_ns.oid = target_tbl.relnamespace
    JOIN unnest(con.conkey) WITH ORDINALITY AS src_colnum(attnum, ord)
        ON TRUE
    JOIN unnest(con.confkey) WITH ORDINALITY AS target_colnum(attnum, ord)
        ON src_colnum.ord = target_colnum.ord
    JOIN pg_attribute src_col
        ON src_col.attrelid = src_tbl.oid
       AND src_col.attnum = src_colnum.attnum
    JOIN pg_attribute target_col
        ON target_col.attrelid = target_tbl.oid
       AND target_col.attnum = target_colnum.attnum
    WHERE con.contype = 'f'
) fk
    ON fk.source_schema = cols.table_schema
   AND fk.source_table = cols.table_name
   AND fk.source_column = cols.column_name
WHERE cols.table_schema IN ($1)
--  AND cols.table_name = $2
  AND a.attnum > 0
  AND tbl.relkind = 'r'
  AND NOT a.attisdropped
ORDER BY cols.table_schema, cols.table_name, cols.ordinal_position;"
    query(connection, sql, params = list(schema_names))
    # result <- lapply(tables, function(table_name) {
    #   gav <- table_location$new(table_name)
    # })
    # names(result) <- tables
    # return(result)
  })
}

extract_db_metadata <- function(domain, version, schemas, connection_provider = connect_to_ros) {
  schemas_comment <- get_schemas_comment(schemas, connection_provider)
  tables_comment <- get_tables_comment(schemas, connection_provider)
  tables_columns <- get_tables_columns(schemas, connection_provider)
  # db_metadata_list <- lapply(schemas, function(x) {
  #   tables <- data.table(tables_comment)[schema == x][, gav := paste0(schema, ".", table)]$gav
  #   get_column_names(tables, connection_provider)
  # })
  # names(db_metadata_list) <- schemas
  db_metadata$new(domain, version, schemas_comment, tables_comment, tables_columns)
}

generate_db_metadata <- function(db_metadata, root_directory) {
  output_directory <- file.path(root_directory, db_metadata$domain())
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  path <- file.path(output_directory, "schemas_comment.csv")
  write_file(db_metadata$to_schema_comments(), path)
  path <- file.path(output_directory, "tables_comment.csv")
  write_file(db_metadata$to_table_comments(), path)

  path <- file.path(output_directory, "tables_column.csv")
  write_file(db_metadata$to_table_columns(), path)
  invisible()
}

load_db_metadata <- function(domain, root_directory, version) {
  output_directory <- file.path(root_directory, domain)
  schemas_comment <- fread(file.path(output_directory, "schemas_comment.csv"))
  tables_comment <- fread(file.path(output_directory, "tables_comment.csv"))
  tables_columns <- fread(file.path(output_directory, "tables_column.csv"))
  db_metadata$new(domain, version, schemas_comment, tables_comment, tables_columns)
}


out_dt <- function(data) {
  datatable(data,
            autoHideNavigation = TRUE,
            rownames = FALSE,
            lazyRender = TRUE,
            fillContainer = FALSE,
            options = list(dom = "t", ordering = FALSE))
}

generate_db_metadata_template <- function(db_metadata, export_directory, template = "./RMDs/ros_metatadata.Rmd") {
  file_location <- file.path(export_directory, sprintf("ros_metatadata-%s.Rmd", db_metadata$domain()))
  if (file.exists(file_location)) {
    file.remove(file_location)
  }
  content <- readLines(template)
  for (schema in db_metadata$all_schemas()) {
    content <- append(content, sprintf("
```{r}
db_schema <- db_metadata$schema('%s')
```

```{r child='ros_metatadata-schema.Rmd'}
```
", schema$schema()))
    # content <- append(content, "")
    for (table in schema$all_tables()) {
      content <- append(content, sprintf("
```{r}
db_table <- db_schema$table('%s')
```
```{r child='ros_metatadata-table.Rmd'}
```
", table$table()))
    }
  }
  writeLines(content, file_location)
  file_location
}

generate_db_metadata_report <- function(db_metadata, export_directory, template = "./RMDs/ros_metatadata.Rmd", timestamp = format_timestamp(Sys.time())) {
  options(DT.options = list(pageLength = -1))
  last_update <- withr::with_locale(c(LC_TIME = "C"), format(Sys.time(), '%d %B %Y %H:%M'))
  file_location <- file.path(export_directory, sprintf("ROS_database_%s%s.html", db_metadata$domain(), timestamp))
  render(template,
         output_format = "html_document",
         output_file = basename(file_location),
         output_dir = dirname(file_location))
}

# db_metadata_ll <- extract_db_metadata("LL", IOTC_ROS, LL_SCHEMAS)
# db_metadata_ps <- extract_db_metadata("PS", IOTC_ROS, PS_SCHEMAS)
# generate_db_metadata(db_metadata_ll, "./models/ROS")
# generate_db_metadata(db_metadata_ps, "./models/ROS")

db_metadata_ll <- load_db_metadata("LL", "./models/ROS", version = IOTC_ROS)
db_metadata_ps <- load_db_metadata("PS", "./models/ROS", version = IOTC_ROS)

template_ll <- generate_db_metadata_template(db_metadata_ll, "./RMDs")
generate_db_metadata_report(db_metadata_ll, "./models/ROS", template = template_ll, timestamp = DEFAULT_TIME_STAMP)
template_ps <- generate_db_metadata_template(db_metadata_ps, "./RMDs")
generate_db_metadata_report(db_metadata_ps, "./models/ROS", template = template_ps, timestamp = DEFAULT_TIME_STAMP)
