library(RMariaDB)
library(DT)
library(DBI)

# Database
test_con <- dbConnect(MySQL::MariaDB(), group = "my-db", dbname="cell-line")