# Cleaning the R environment
rm(list = ls())
# Set Working Directory to appropriate folder
# Assumption : All required packages are pre-installed in your R environment else use install.packages() to install these packages before running the code
# Load required libraries
library(data.table)
library(gdata)
library(RMySQL)
library(tidyr)
library(dplyr)
library(sqldf)
library(stringr)
library(xlsx)

# checking for files in working directory and downloading if not present
if (!file.exists("companies.txt")){
  companies_url = "https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt"
  download.file(companies_url,"companies.txt",mode = "wb")
  rm(companies_url)
}
if (!file.exists("mapping.csv")){
  mapping_url = "https://cdn.upgrad.com/UpGrad/temp/231dc91c-0642-470d-a362-29ddcd7142ce/mapping.csv"
  download.file(mapping_url,"mapping.csv",mode = "wb")
  rm(mapping_url)
}
if (!file.exists("rounds2.csv")){
  rounds_url = "https://cdn.upgrad.com/UpGrad/temp/4c3b5ed0-e5dc-4838-89a2-173d8707d857/rounds2.csv"
  download.file(rounds_url,"rounds2.csv",mode = "wb")
  rm(rounds_url)
}

# listing the files in the directory - Confirmation to ensure all required dataset is available to start viewing and preparation

list.files() 

## ============================== DATA COLLECTION ============================================

# Load the companies and rounds data into two data frames and name them companies and rounds2 respectively.
# As given dataset is txt file with many missing fields in various columns, it is good to use read.delim function with fill = TRUE

companies <- read.delim("companies.txt", header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)

# Alternate Approach by using different library and functions

companies = fread("companies.txt") # companies dataset
# used na.strings = "" to replace blank values in dataset
# str(companies)
# head(companies)

rounds2 = fread("rounds2.csv") # rounds2 dataset
# used na.strings = "" to replace blank values in dataset
# str(rounds2)
# head(rounds2)

mapping = fread("mapping.csv")
# head(mapping)

# In additional to these given data files we can convert the provided PDF file to read all English speaking countries and by using datasource from wikipedia, it can be integrated with master_frame



## =============================== DATA PREPRATION ============================================

# 1) The unique column which is the companies ID column has the letters in both upper
# and lower case. same with rounds2 dataset. We have to convert them to lower case,
# so that our analysis becomes esiaer and merging too. Let's do this.

companies$permalink = str_to_lower(companies$permalink)
rounds2$company_permalink = str_to_lower(rounds2$company_permalink)


# 2) Spelling mistakes in mapping data

string_clean = function(x){
  if (str_detect(x,"0$")){
    x = x
  }else {
    sub("0","na",x)
  }
}

mapping$category_list = sapply(mapping$category_list, function(x){string_clean(x)})


# =================================      CHECKPOINT 1     =====================================#

# 1. How many unique companies are in round2?
length(unique(rounds2$company_permalink)) 
# 66368 unique companies in rounds2

# 2. How many unique companies are present in companies?
length(unique(companies$permalink)) 
# 66368 unique companies in companies dataframe

# 3. In the companies data frame, which column can be used as the unique key for each company?
sapply(companies,function(x) length(unique(x))== nrow(companies))
# As it was observed that column "permalink" has all unique values, it can be used as Unique Key for each company

# 4. Are there any companies in the rounds2 file which are not present in companies?
sum(!(rounds2$company_permalink %in% companies$permalink))
# As the outcome is 0, it is identified that there is no additional company present in round2 which is not present company dataframe.

# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
# 5. How many observations are present in master_frame?
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")
nrow(master_frame)
# master_frame dataframe has 114949 observations of 15 variables


# ================================   CHECKPOINT 2  ==============================================

# Funding Type Analysis
# Average funding amount of venture type
# Average funding amount of angel type
# Average funding amount of seed type
# Average funding amount of private equity type

View(master_frame)

investment_type_group <- group_by(master_frame, funding_round_type)
average_funding_by_funding_type <- summarise(investment_type_group, avg_funding = mean(raised_amount_usd, na.rm = TRUE))


# 1. Average funding amount of venture type
filter(average_funding_by_funding_type, funding_round_type == 'venture')$avg_funding
# Average Funding amount of Venture type is 11748949

# 2. Average funding amount of angel type
filter(average_funding_by_funding_type, funding_round_type == 'angel')$avg_funding
# Average Funding amount of Angel type is 958694.5

# 3. Average funding amount of seed type
filter(average_funding_by_funding_type, funding_round_type == 'seed')$avg_funding
# Average Funding amount of Seed type is 719818

# 4. Average funding amount of private equity type
filter(average_funding_by_funding_type, funding_round_type == 'private_equity')$avg_funding
# Average Funding amount of private equity type is 73308593

# Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
# which investment type is the most suitable for them?
filter(average_funding_by_funding_type, avg_funding >= 5000000, avg_funding <= 15000000)$funding_round_type
# Recommended funding type is VENTURE

# ==============================    CHECKPOINT 3   =======================================

# Spark Funds wants to see the top nine countries which have received the highest total funding 
#(across ALL sectors for the chosen investment type)
#For the chosen investment type, make a data frame named top9 with the top nine countries 
#(based on the total investment amount each country has received)


df_venture <- filter(master_frame, funding_round_type =="venture")
df_venture <- df_venture[!df_venture$country_code == "",] # Removing rows with blank country code
grp_country <- group_by(df_venture, country_code)
countrywise_funding <- summarise(grp_country, total_funding = sum(raised_amount_usd, na.rm = T))
top9 <- arrange(countrywise_funding, desc(total_funding))[1:9,]
View(top9)

# Top English speaking country - USA
# Second English speaking country - GBR
# Third English speaking country - IND

# NOTE : we referred to the provided PDF file to identify the Only English speaking companies manually. 
# However, it is possible to extract that PDF to a df and compare it with the data extracted from wikepedia to integrate it with the masterframe and apply filters only on English speaking countries


# ==============================    CHECKPOINT 4   =======================================

# Extract the primary sector of each category list from the category_list column
# Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
# (Note that 'Others' is also considered one of the main sectors)
# Expected Results: Code for a merged data frame with each primary sector mapped to its main sector 
# (the primary sector should be present in a separate column).

master_frame <- master_frame %>% separate(category_list,"primary_sector","\\|",remove = FALSE)
mapping_long <- gather(mapping, "main_sector", "count", 2:10)
mapping_long <- filter(mapping_long, count != 0)
mapping_long <- mapping_long[,-3]

# as both the columns on dataframes are case sensitive, converting this column to lower case
master_frame$primary_sector <- tolower(master_frame$primary_sector)
mapping_long$category_list <- tolower(mapping_long$category_list)

# merging mapping file to master_frame to have one dataframe will all required inputs
master_frame_sector <- merge(master_frame, mapping_long, by.x = "primary_sector", by.y = "category_list", all.x = TRUE)
# master_frame_sector is the consolidated dataframe with all primary sector mapped to its main sector in one file

# ============================   Checkpoint 5: Sector Analysis 2   ===================================== 

# Using Dice by filtering the dataframe on Investment type, Country and Invesment amount to create a more specific dataframes.

D1 <- filter(master_frame_sector, master_frame_sector$country_code == "USA" & master_frame_sector$funding_round_type == "venture" & master_frame_sector$raised_amount_usd >= 5000000 & master_frame_sector$raised_amount_usd <= 15000000)
D2 <- filter(master_frame_sector, master_frame_sector$country_code == "GBR" & master_frame_sector$funding_round_type == "venture" & master_frame_sector$raised_amount_usd >= 5000000 & master_frame_sector$raised_amount_usd <= 15000000)
D3 <- filter(master_frame_sector, master_frame_sector$country_code == "IND" & master_frame_sector$funding_round_type == "venture" & master_frame_sector$raised_amount_usd >= 5000000 & master_frame_sector$raised_amount_usd <= 15000000)

# 1. Total number of investments (count)

no_investment <- data.frame(length(D1$raised_amount_usd), length(D2$raised_amount_usd), length(D3$raised_amount_usd))
no_investment

# 12150 : 628 : 330

# 2. 2. Total amount of investment (USD)

total_investment <- data.frame(sum(D1$raised_amount_usd), sum(D2$raised_amount_usd), sum(D3$raised_amount_usd))
total_investment

#  $108,531,347,515.00 	 $5,436,843,539.00 	 $2,976,543,602.00 

main_sector_group_D1 <- group_by(D1, main_sector)
main_sector_inv_D1 <- summarise(main_sector_group_D1, count_of_investment = length(main_sector))
top3_sector_D1 <- head(main_sector_inv_D1[order(-main_sector_inv_D1$count_of_investment), ],3)
View(top3_sector_D1)

main_sector_group_D2 <- group_by(D2, main_sector)
main_sector_inv_D2 <- summarise(main_sector_group_D2, count_of_investment = length(main_sector))
top3_sector_D2 <- head(main_sector_inv_D2[order(-main_sector_inv_D2$count_of_investment), ],3)
View(top3_sector_D2)

main_sector_group_D3 <- group_by(D3, main_sector)
main_sector_inv_D3 <- summarise(main_sector_group_D3, count_of_investment = length(main_sector))
top3_sector_D3 <- head(main_sector_inv_D3[order(-main_sector_inv_D3$count_of_investment), ],3)
View(top3_sector_D3)

# 3. Top Sector name (no. of investment-wise)
top_sec_count_of_investment <- c(top3_sector_D1[1,1], top3_sector_D2[1,1], top3_sector_D3[1,1])
top_sec_count_of_investment

# others : others : Others

# 4. Second Sector name (no. of investment-wise)
top_2nd_count_of_investment <- c(top3_sector_D1[2,1], top3_sector_D2[2,1], top3_sector_D3[2,1])
top_2nd_count_of_investment

# Social..Finance..Analytics..Advertising : Social..Finance..Analytics..Advertising : Social..Finance..Analytics..Advertising

# 5. Third Sector name (no. of investment-wise)
top_3rd_count_of_investment <- c(top3_sector_D1[3,1], top3_sector_D2[3,1], top3_sector_D3[3,1])
top_3rd_count_of_investment
# Cleantech...Semiconductors  : Cleantech...Semiconductors : News..Search.and.Messaging

# 6. Number of investments in top sector (3)
top_count_of_investment <- c(top3_sector_D1[1,2], top3_sector_D2[1,2], top3_sector_D3[1,2])
top_count_of_investment
# 2950 : 147 : 110

# 7. Number of investments in second sector (4)
top2_count_of_investment <- c(top3_sector_D1[2,2], top3_sector_D2[2,2], top3_sector_D3[2,2])
top2_count_of_investment
# 2714 : 133 : 60

#8. Number of investments in third sector (5)
top3_count_of_investment <- c(top3_sector_D1[3,2], top3_sector_D2[3,2], top3_sector_D3[3,2])
top3_count_of_investment
# 2350 : 130 : 52 

#9. For point 3 (top sector count-wise), which company received the highest investment?

D1_Top_Sec_df <- filter(D1, D1$main_sector == top_sec_count_of_investment[1])
D1_company_grp <- group_by(D1_Top_Sec_df, name)
D1_top_company <- summarise(D1_company_grp, total_inv = sum(raised_amount_usd))
D1_others_company_max_investment <- arrange(D1_top_company, desc(total_inv))[1,1]

D2_Top_Sec_df <- filter(D2, D2$main_sector == top_sec_count_of_investment[2])
D2_company_grp <- group_by(D2_Top_Sec_df, name)
D2_top_company <- summarise(D2_company_grp, total_inv = sum(raised_amount_usd))
D2_others_company_max_investment <- arrange(D2_top_company, desc(total_inv))[1,1]

D3_Top_Sec_df <- filter(D3, D3$main_sector == top_sec_count_of_investment[3])
D3_company_grp <- group_by(D3_Top_Sec_df, name)
D3_top_company <- summarise(D3_company_grp, total_inv = sum(raised_amount_usd))
D3_others_company_max_investment <- arrange(D3_top_company, desc(total_inv))[1,1]

others_companies_max_investment <- data.frame(D1_others_company_max_investment, D2_others_company_max_investment, D3_others_company_max_investment)
others_companies_max_investment

# Virtustream : Electric Cloud : FirstCry.com


#For point 4 (second best sector count-wise), which company received the highest investment?
D1_2_top_sec <- filter(D1, D1$main_sector == top_2nd_count_of_investment[1])
D1_2_company_grp <- group_by(D1_2_top_sec, name)
D1_2_top_company <- summarise(D1_2_company_grp, total_inv = sum(raised_amount_usd))
D1_2_company_max_investment <- arrange(D1_2_top_company, desc(total_inv))[1,1]

D2_2_top_sec <- filter(D2, D2$main_sector == top_2nd_count_of_investment[2])
D2_2_company_grp <- group_by(D2_2_top_sec, name)
D2_2_top_company <- summarise(D2_2_company_grp, total_inv = sum(raised_amount_usd))
D2_2_company_max_investment <- arrange(D2_2_top_company, desc(total_inv))[1,1]

D3_2_top_sec <- filter(D3, D3$main_sector == top_2nd_count_of_investment[3])
D3_2_company_grp <- group_by(D3_2_top_sec, name)
D3_2_top_company <- summarise(D3_2_company_grp, total_inv = sum(raised_amount_usd))
D3_2_company_max_investment <- arrange(D3_2_top_company, desc(total_inv))[1,1]

sec_sector_companies_max_investment <- data.frame(D1_2_company_max_investment, D2_2_company_max_investment, D3_2_company_max_investment)
sec_sector_companies_max_investment

# SST Inc. (Formerly ShotSpotter) : Celltick Technologies  : Manthan Systems

# To have Tableau View, exporting the dataframe to csv file
write.csv(master_frame_sector, file = "investment_data.csv")


