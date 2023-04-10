
#----------------------------
# RSQLite (on-disk DB)
#----------------------------

# store permanently on disk
conn <- dbConnect(RSQLite::SQLite(), "appdata.sqlite")

# write data to table
RSQLite::dbWriteTable(conn, "App_data", app_data)

for (name in names(app_data)){
  table_name <- paste0("tbl_", name)
  dbWriteTable(conn, table_name, app_data[[name]])
}

dbListTables(conn) # check what tables exist in db
dbReadTable(conn, "App_data") # see table contents
dbRollback(conn) # restore previous version of a db

