library(odbc)
library(DBI)

hsql <- function(q, db = "nhs_reporting", server = "WWLDWDEV01"){
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = server,
                   Database = db)
  result <- try(dbGetQuery(con, q))
  
  dbDisconnect(con)
  return(result)
} 

getTables <- function(db = "nhs_reporting", server = "WWLDWDEV01"){
  con <- dbConnect(odbc(),
                   Driver = "SQL Server",
                   Server = server,
                   Database = db)
  tables <- try(dbListTables(con))
  dbDisconnect(con)
  return(tables)
}

hsqlTable <-
  function (value,
            server = "wwldevsqlfrm1",
            database = NA,
            tablename = tablename)
  {
    con = dbConnect(odbc(),
                    Driver = "SQL Server",
                    Server = server,
                    Database = database)
    tryWrite <- try(DBI::dbWriteTable(
      conn = con,
      name = tablename,
      value = value,
      append = T,
      row.names = F
    ))
    DBI::dbDisconnect(conn = con)
    return(tryWrite)
  }

