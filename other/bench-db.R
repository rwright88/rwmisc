# bench database

library(rwmisc)

system.time({
  rwmisc::db_write_files(
    files = list.files("~/data/trans", pattern = "transactions", full.names = TRUE),
    file_db = "~/data/trans/transdb",
    table_name = "trans",
    batch_size = 2
  )
})
