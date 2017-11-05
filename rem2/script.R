#install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  ""
}
 
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "rem2",
                 host = "localhost", port = 5432,
                 user = "mee", password = pw)
rm(pw) # removes the password

# Alle Mietvertraege selektieren

df_postgres <- dbGetQuery(con, "SELECT id mietbeginn, haftung_bis, zahlmodus_cd, verbucht_bis, type, hauptmieter_id, hauptobjekt_id, mietvertrag_id from mietvertraege limit 10")

 
# check for the cartable
dbExistsTable(con, "mietvertraege")
# TRUE