

#devtools::install("../rapbase", upgrade = FALSE)

#hver gang man har gjort endringer
devtools::install(".", upgrade = FALSE) #installerer pakken


#Kjør app
source("dev/renv.R")  #kan slås sammen med den under
source("dev/renv_mssql.R")
hjerteinfarkt::run_app(browser = TRUE)

################
# MSSQL-greier #
################

hjerteinfarkt::run_app(browser = TRUE)

con <- rapbase::rapOpenDbConnection("autoreport")$con

conf <- rapbase:::getDbConfig("autoreport")

install.packages("odbc")
con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "FreeTDS",
  database = conf$name,
  server = conf$host,
  uid = conf$user,
  pwd = conf$pass,
  port = 1433
)

con <- DBI::dbDisconnect(con)
con <- NULL
