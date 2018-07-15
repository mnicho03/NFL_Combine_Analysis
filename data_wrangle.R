#Gather NFL Data

# #set working directory
# setwd("../Desktop/Practice/Fuhball")

#download libraries
library(rvest) # for web scraping
library(lubridate) # for dates
library(stringr) # for text manipulation
library(data.table) # for dataframe manipulation

#assign draft data URL to variable and scrape HTML for the table using rvest package
draftURL <- "https://www.pro-football-reference.com/draft/draft-totals.htm"
draftOverview <- read_html(draftURL)

#scrape draft data and assign to data frame
Draft_Overview_Data <- draftOverview %>% 
        html_node("table") %>% 
        html_table(header = TRUE, fill = TRUE)

#current state: scrapes default page (2017 only)
#future state: run through each page (1987-2016) and append to same data frame
##http://nflcombineresults.com/nflcombinedata.php?year=1987&pos=&college=
## through
## http://nflcombineresults.com/nflcombinedata.php?year=2017&pos=&college=

#save combine data URL
combineURL <- read_html("http://nflcombineresults.com/nflcombinedata.php")

#scrape combine data and assign to data frame
combineData_2018 <- combineURL %>% 
        html_node("table") %>% 
        html_table(header = TRUE, fill = TRUE)

#combine historical (1987-2016) - create list of URL's for each year
combineURLs_Historical <- sprintf("http://nflcombineresults.com/nflcombinedata.php?year=%d&pos=&college=", 1987:2017)

#scrape historical combine data and assign to data frame
scrape_combine_data <- function(url) {

                page <- read_html(url)
                data <- page %>%
                        html_node("table") %>%
                        html_table(header = TRUE, fill = TRUE)
                
                year <- as.character(str_extract_all(url, "[[:digit:]]{4}"))
                
                #assign data to custom name by year
                assign(paste0("combineData_", year), data, envir = .GlobalEnv)
}


#lapply function to apply it across each URL
#creates data frames for each combine 1987-2017
lapply(combineURLs_Historical, scrape_combine_data)

#create variable listing data frames from each yr
all_combine_datasets <- ls(pattern = "combineData_")

#lapply again to apply the filter function across each year
for (i in 1:length(all_combine_datasets)) {
        
                #check which columns are empty and create a subset excluding them
                #save to new var
                clean_combine_year <- get(all_combine_datasets[i])[, !apply(is.na(get(all_combine_datasets[i])), 2, all)]
                
                #save off new variable to custom name by year
                #deparse(substitute( used to grab just the name of the DF
                assign(paste0("clean_", all_combine_datasets[i]), clean_combine_year, envir = .GlobalEnv)
                
                #remove clean_combine_year var
                rm(clean_combine_year)
        }
        
#remove data frames with the extra columns (and the list containing them)
rm(list = all_combine_datasets, all_combine_datasets)

#remove all non-data variables
rm(list = setdiff(ls(), ls(pattern = "Data")))

#merge to create one full dataframe
#fill = TRUE ~ returns full columns of NA when they do not appear in certain dataframes (e.g. Wonderlic scores prior to '97)
all_combine_data <- rbindlist(mget(ls(pattern = "combineData_")), fill = TRUE)

#remove individual combine dataframes
rm(list = ls(pattern = "combineData_"))

#write out CSV's for the final data
fwrite(all_combine_data, "Data/all_combine_data.csv")
fwrite(Draft_Overview_Data, "Data/draft_overview_data.csv")

###more cleaning! searching for NA's
#show dataset summaries
str(all_combine_data)

#convert to data table
all_combine_data <- as.data.table(all_combine_data)

#clean up names
names(all_combine_data) <- c("year", "name", "college", "position", "height_inches", "weight_lbs", "forty_yard", "bench_press", "vertical_inches", "broad_jump_inches", "shuttle", "three_cone", "wonderlic")

#numeric columns without legitimate records appear to be listed as '9.99'
#determine counts of each
length(all_combine_data[all_combine_data$forty_yard == 9.990,]$forty_yard)
length(all_combine_data[all_combine_data$shuttle == 9.990,]$shuttle)
length(all_combine_data[all_combine_data$three_cone == 9.990,]$three_cone)

#convert dirty records (9.990 times) to NA
all_combine_data[forty_yard == 9.990, forty_yard := NA]
all_combine_data[shuttle == 9.990, shuttle := NA]
all_combine_data[three_cone == 9.990, three_cone := NA]

#look for columns with NA for year
all_combine_data[is.na(all_combine_data$year),]

##shows 32 rows with all NA: can delete --- or in the case below, create the new dataset excluding those rows
all_combine_data <- all_combine_data[!is.na(all_combine_data$year),]

#show data summary
summary(all_combine_data)

#convert all integers to numeric
all_combine_data$weight_lbs <- as.numeric(all_combine_data$weight_lbs)
all_combine_data$bench_press <- as.numeric(all_combine_data$bench_press)
all_combine_data$broad_jump_inches <- as.numeric(all_combine_data$broad_jump_inches)
all_combine_data$wonderlic <- as.numeric(all_combine_data$wonderlic)



#remaining players have at least year, name, college, position, height, weight
#save off this to the data folder
fwrite(all_combine_data, "Data/clean_combine_data.csv")
