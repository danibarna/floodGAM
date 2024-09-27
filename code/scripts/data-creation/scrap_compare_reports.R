##
##
##
##
##
##


library(data.table)


nifs.tab <- fread("~/floodGAM/data/raw-data/NIFS_stations_table_5.txt",
                  fill=T, colClasses = list(character=1))

A2.tab <- fread("~/floodGAM/data/raw-data/rapport2016_85_tabell_A2.txt",
                  fill=T, colClasses = list(character=1))

A3.tab <- fread("~/floodGAM/data/raw-data/rapport2016_85_tabell_A3_list_over_utelatt_fra_flomdatasett.txt",
                  fill=T, colClasses = list(character=1))

sma.tab <- fread("~/floodGAM/data/raw-data/rapport2013_66_tabell_1.txt",
                          fill=T, colClasses = list(character=1))

















