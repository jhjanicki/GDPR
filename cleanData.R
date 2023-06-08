library(tidyverse)

data <- read_csv("GDPR.csv")

# create a "year" column to store the year, create a "Fine_cleaned" column to keep only the numbers while removing the text
dataProcessed <- data %>% mutate(year = substring(Date, 1, 4)) %>% mutate(Fine_cleaned = if_else(grepl("^\\d+$", Fine_cleaned), Fine_cleaned, NA)) %>% arrange(desc(Fine_cleaned))

# change "Fine_cleaned" column to numeric
dataProcessed$Fine_cleaned <- as.numeric(as.character(dataProcessed$Fine_cleaned))

# country / year
groupByCountryYearSum <- dataProcessed %>% group_by(Country,year) %>% summarise(total=sum(Fine_cleaned,na.rm = TRUE))

# country
groupByCountrySum <- dataProcessed %>% group_by(Country) %>% summarise(total=sum(Fine_cleaned,na.rm = TRUE))

# top 15 countries
groupByCountrySum15 <- groupByCountrySum %>%arrange(desc(total)) %>%mutate(Country = ifelse(row_number() <= 15, Country, "other"))

# type of infraction
groupByType <- dataProcessed %>% group_by(Type) %>% summarise(total=sum(Fine_cleaned,na.rm = TRUE))

# type / country
groupByTypeCountry <- dataProcessed %>% group_by(Type,Country) %>% summarise(total=sum(Fine_cleaned,na.rm = TRUE))

# clean companies, only keep top companies while set the rest to "other"
dataProcessedCompany <- dataProcessed %>% mutate(Controller_cleaned = case_when(
       str_detect(Controller, "Google") ~ "Google",
       str_detect(Controller, "Meta|Facebook|WhatsApp") ~ "Meta",
       str_detect(Controller, "Amazon") ~ "Amazon",
       str_detect(Controller, "H&M") ~ "H&M",
       str_detect(Controller, "Vodafone") ~ "Vodafone",
       str_detect(Controller, "Grindr") ~ "Grindr",
       str_detect(Controller, "TikTok")~ "TikTok",
       str_detect(Controller, "Clearview") ~ "Clearview", 
       str_detect(Controller, "TIM") ~ "TIM",
       str_detect(Controller, "British Airways") ~ "British Airways",
       str_detect(Controller, "Marriott") ~ "Marriott",
       TRUE ~ "other"
   ))

# top 15 countries
countries <- c("LUXEMBOURG","IRELAND","FRANCE","GERMANY","ITALY","UNITED KINGDOM","SPAIN","AUSTRIA","NORWAY","PORTUGAL","POLAND","BULGARIA","SWEDEN","GREECE","THE NETHERLANDS")
dataProcessedCompany<- dataProcessedCompany %>% mutate(Country2 = ifelse(Country %in% countries, Country, "other"))

write_csv(dataProcessed,"data_cleaned.csv")
write_csv(dataProcessedCompany,"data_company_country.csv")

