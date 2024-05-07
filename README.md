# Cell Line Database

Database management system for cell lines. The app is built using Shiny App and the R language, with the help of MySQLin the database side. 

## Table of Contents

-   [To-do](#to-do)
-   [How to run the app locally](#how-to-run-the-app-locally)
-   [How to run the app in cloud](#how-to-run-the-app-in-cloud)
-   [Front end](#front-end)
-   [Preprocessing](#preprocessing)
-   [Data files](#data-files)
-   [Database](#database)

## To-do

-   [ ] Properly connect MySQL with the front end
-   [ ] Online deployment 
-   [ ] User authentication (with [shinyauthr](https://rdrr.io/github/PaulC91/shinyauthr/)?)
-   [ ] There might be a problem with the data/server not loading properly in the new version of the app that divides server, ui etc. to their own files

## How to run the app locally

The easiest way is to run the app locally. In the database folder how to get the MySQl part working is talked about. Note that the proper integration is still a work-in-progress. The steps required to get it to work with RStudio are as follows:

1.  Download RStudio together with R 
2.  Clone the repository 
3.  Open the app.R file in RStudio
4.  Click on the install package pop-ups
5.  Click on the 'Run App' button
6.  Enjoy! 

Note that RStudio is obviously not the only way of getting it up and running, but is the easiest way of testing the application due to RStudio doing a lot of the heavy lifting. To run the Census-seq app, you do not need MySQL at all, just open its app.R file and run it. However, do not forget to add the two required data files before doing this inside census-seq folder.

Getting the database to work might be difficult this way and require considerable effort. One approach to try to streamline the process is to use Docker and more specifically Docker's official MySQL [image](https://hub.docker.com/_/mysql). See, for example, the following [tutorial](https://www.howtogeek.com/devops/how-to-run-mysql-in-a-docker-container/).

## How to run the app in cloud

Currently, this implementation is not completed. Please refer to database folder's readme.md file. 

## Front end

The front end uses [Shiny App for R](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html). The file app.R is used for running the application and loading packages. The server and ui functonalities have been extracted to their own files to make everything more clear. The global.R file is used to load functionality before any other file is run. The package responsible for the look of the application is [shinydashboard](https://rstudio.github.io/shinydashboard/index.html). Please read the tutorial linked in the previous sentence to better understand how it works. To better understand databases with Shiny App (it is for SQLite, but still informative) refer to the [following tutorial](https://shanghai.hosting.nyu.edu/data/r/case-4-database-management-shiny.html). The package [shinyauthr](https://rdrr.io/github/PaulC91/shinyauthr/), which was also talked about in the previously linked tutorial, is useful for creating user authentications. 

## Preprocessing

The preprocessing folder contains Python file used for some automatic tools used to help preprocessing.

## Data files

The data files for the main database, Census-seq, and Census-seq's meta data are not in included in the repository for potential privacy reasons (though it should be noted that the repository is private). This is also why their names appear in the .gitignore file. Put the data in either the application/data folder if you want to use the application with it, or in census-seq folder for Census-seq related tasks. The application/data folder has dummy data that is not sensitive.  

## Database

Details about database development, including how to convert to the data to a database, can be found in the readme.md file of the database. 