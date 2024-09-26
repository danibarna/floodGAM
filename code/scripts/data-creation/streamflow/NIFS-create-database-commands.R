##
##
##
##
## Create lescon_var script for calling NIFS stations
## uses Table 5 in report 13-2015 
## 'Nasjonalt formelverk for flomberegning i små nedbørfelt'
## 
## Compare to table A2 from report 2016:85
## -----------------------------------------------------------------------------

library(data.table)
library(stringr)

## read in the table
nifs.tab <- fread("~/floodGAM/data/raw-data/NIFS_stations_table_5.txt",
                  fill=T, colClasses = list(character=1:20))

## first column is station numbers 





