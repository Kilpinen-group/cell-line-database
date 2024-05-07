# Database implementation

## Table of Contents

-   [Overall status](#overall-status)
-   [Steps forward](#steps-forward)
-   [Database systems considered](#database-systems-considered-with-the-r-frontend)
-   [Packages for using the databases through R](#packages-for-using-the-databases-through-r)
-   [SQLite implementation](#sqlite-implementation)
-   [Current approach: MySQL](#current-approach-mysql)
    -   [About implementation](#about-implementation)
    -   [How to implement](#how-to-implement)
    -   [How to connect MySQL with ShinyApp in R](#how-to-connect-mysql-with-shinyapp-in-r)
    -   [Example query](#example-query)

## Overall status

-   MySQL database is running locally. (Go [here](#current-approach-mysql) for the detailed implementation)
    -   Connection to R works (Using MariaDB). Populating the database with the Python script works (Using Migration.ipynb).
    -   Another approach using SQLite: has not been connected to R yet. 
-	The schema does not have support for allowed values.
-	We have used the ShinyApp [documentation](https://shiny.posit.co/r/articles/build/persistent-data-storage/) as guideline

## Steps forward

-	Implement function in app.R taking data from database, instead of the dummy csv file. 

## Database systems considered (with the R frontend):

-	Excel
    -	It is not sure if packages would offer good support for writing and reading from the file and how they would behave with multiple users.
    -	Packages for R support reading and writing to Excel but writing the information to the Excel file in the format that each sheet has several tables seems to be hard with the packages. Not sure if writing to an Excel file stored in OneDrive would be possible with these.
-	Microsoft Lines
    -	Microsoft Office 365 tool to store lines of data to SharePoint; a lines document can be made from Excel or csv directly. 
    -	Seems promising but not researched enough for implementing. Microsoft Lines would allow data handling through the Lines interface and the R application could be maybe used just for the data visualizations. Lines supports creating views/dashboards.
-	MySQL
    -	Great for a real web-app and for providing support for multiple users. However, it needs a server for running this. Currently running it on localhost, so all of the users would need to set up the system. 
    -	The optimal case would be to use a server on csc where users need a csc account. 
-	PostgreSQL
    -	Mostly the same as MySQL – we have not tried this out, but for example the Database Application Project course in the University of Helsinki’s CS Bachelor’s program uses this.
-	SQLite
    -	Runs locally, and the whole database is only one file. Easy to use, install, and manage. Supports only one person writing to the database at one time.
    -	Probably not a good idea to store it in a shared OneDrive or similar, but this is a reasonable option for developing the app locally.

## Packages for using the databases through R

The following examples seem to be good fits:

-	Excel
    -	[readxl](https://cran.r-project.org/web/packages/readxl/index.html)
    -	[xlsx](https://cran.r-project.org/web/packages/xlsx/readme/README.html) (seems kind of old)
-	Microsoft Lines
    -	[Microsoft365R](https://cran.r-project.org/web/packages/Microsoft365R/)
-	MySQL
    -	ShinyApp documentation refers to [RMySQL](https://github.com/r-dbi/RMySQL), but it is phased out and the new one replacing it is [RMariaDB](https://github.com/r-dbi/RMariaDB)
-	PostgreSQL
    -	[RPostgreSQL](https://cran.r-project.org/web/packages/RPostgreSQL/index.html)
-	SQLite
    -	[RSQLite](https://cran.r-project.org/web/packages/RSQLite/)

## SQLite implementation

-	There is a Python script to create the database and populate it from the Excel file using the old schema (the old schema does not consider the allowed values, so the schema needs to be updated to support that).
    -	Script is available in migration.ipynb, and can be converted to using SQLite using the comments in the notebook.
-	Connecting SQLite to R has not yet been tested. 

## Current approach: MySQL

### About implementation

-	Seems to require a lot of configurations which depend on the operating system to get the database to run locally.
-	Connection the database to R using MariaDB is established, and it is running locally.

### How to implement

1.	Set up a MySQL server locally. Create an empty MySQL database.
2.	Initialize the tables in the database based on the database architecture. You can use dbmysql.txt or follow the instructions under create table section in Migration.ipynb. 
3.	Populate the empty database with data provided (the excel file). Follow instructions in Migration.ipynb and use the knowledge of the changes.txt file to make sure the Excel file is correct.
4.	Connect the mysql database with ShinyApp in app.R using MariaDB, [https://github.com/r-dbi/RMariaDB?tab=readme-ov-file](https://github.com/r-dbi/RMariaDB?tab=readme-ov-file) 

We have faced problems when installing RMariaDB and DBI.

Running this command has solved the issue : sudo apt-get install -y libmysqlclient-dev

### How to connect MySQL with ShinyApp in R

-   install.package("RMariaDB")
-   install.package("DBI")
-   con <- dbConnect(RMariaDB::MariaDB(), group = "my-db", dbname=<FILL IN>,
                    host='localhost',
                      user='root',
                    password=<FILL IN>)

To test if connection is established:

dbListTables(test_con)

### Example query 

dbGetQuery(test_con, "SELECT * FROM Vial;")

dbExecute(test_con, "INSERT INTO table_name('val1', 'val2');")

