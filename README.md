# Migrations-revisions-NHB


Skip over lines 1-29 to recrun models in the 'Nature HB revisions.r' file and just use read in the 'full sample.rds' file.  The first 29 lines juts how how the 
original data have been filtered to get this truncated file that is used to run the main 2 models.

In the 'revised figures.r' file skip the first 41 lines and read in 'full sample.rds' as 'm'.  

In other words type in: "m<- readRDS(path/full_sample.rds) and then the rest of the code should work properly for making all figures and running the models.

Note that the figure files require you to read in very large model.rds files which could not be added to Githb die to their size.

Therefore these models must be generated first by running the 'Nature HB revisions.r' code and then saving the models and then using these
files to generate the figures.