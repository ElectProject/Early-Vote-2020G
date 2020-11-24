library(tidyverse)
library(readxl)
library(stringr)
library(scales)
library(rvest)

###################
# Setup
###################


#######################
# Georgia
#######################

GA_report_date <- "11/23/2020"

# Start here if already concatenated county files

GA_2020ro <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/GA/2020RO/STATEWIDE.csv")

names(GA_2020ro)<-str_replace_all(names(GA_2020ro), c(" " = "."))
names(GA_2020ro)<-str_replace_all(names(GA_2020ro), c("#" = ""))

GA_2020ro <- GA_2020ro %>%
  rename(REGISTRATION_NUMBER = Voter.Registration.)

voter.file <- "D:/DropBox/Dropbox/Voter Files/GA/20201015/GA Voter List - Statewide - Alpha List.csv"
# readcolumns <- "ccccc_________cccc__c_________________________cccccc___________"
readcolumns <- "ccccccccccccccccccccccccccccc"
GA_vr_file <- read_csv(voter.file, col_types = readcolumns)

# GA_vh_2016g <- read_fwf("D:/DropBox/Dropbox/Voter Files/GA/32668.TXT", fwf_widths(c(3,8,8,3,2,1,1,1)), col_types = "cccccccc")

GA_vh_2020g <- read_fwf("D:/DropBox/Dropbox/Mail_Ballots_2020/GA/2020G/35209.TXT", fwf_widths(c(3,8,8,3,2,1,1,1)), col_types = "cccccccc")

GA_vh_2020g <- GA_vh_2020g %>%
  mutate(voted_2020g = 1) %>%
  rename(REGISTRATION_NUMBER = X2) %>%
  select(REGISTRATION_NUMBER, voted_2020g)

GA_2020ro <- left_join(GA_2020ro, GA_vr_file, by = "REGISTRATION_NUMBER")
GA_2020ro <- left_join(GA_2020ro, GA_vh_2020g, by = "REGISTRATION_NUMBER")

GA_2020ro_app_accept <- GA_2020ro %>%
  filter(Application.Status == "A")

GA_2020ro_app_reject <- GA_2020ro %>%
  filter(Application.Status == "R")

GA_2020ro_app_accept_list <- GA_2020ro_app_accept %>%
  select(REGISTRATION_NUMBER) %>%
  mutate(APP_ACCEPT = "Y")

GA_2020ro_app_reject <- left_join(GA_2020ro_app_reject, GA_2020ro_app_accept_list, by = "REGISTRATION_NUMBER")

GA_2020ro_app_reject <- GA_2020ro_app_reject %>%
  filter(is.na(APP_ACCEPT))

GA_2020ro_app_reject_most_recent <- GA_2020ro_app_reject %>%
  arrange(REGISTRATION_NUMBER, Application.Date) %>%
  group_by(REGISTRATION_NUMBER) %>%
  summarise(recent = last(Application.Date), applications=n()) %>%
  mutate(app_recent = "Y") %>%
  rename(Application.Date = recent) %>%
  mutate(date_added = GA_report_date)

GA_2020ro_app_reject_unique <- left_join(GA_2020ro_app_reject, GA_2020ro_app_reject_most_recent, by = c("REGISTRATION_NUMBER", "Application.Date"))

GA_2020ro_app_reject_unique <- GA_2020ro_app_reject_unique %>%
  filter(!is.na(app_recent))

write_csv(GA_2020ro_app_reject_unique, "D:/DropBox/Dropbox/Rejected_Ballots/GA_RO_Applications_Rejected_Statewide.csv")

GA_2020ro_app_reject_unique_ag <- GA_2020ro_app_reject_unique %>%
  filter(BIRTHYEAR>1995)

write_csv(GA_2020ro_app_reject_unique_ag, "D:/DropBox/Dropbox/Rejected_Ballots/GA_RO_AG_Applications_Rejected_Statewide.csv")

GA_FIPS <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/GA_FIPS.csv", col_types = "cc")

GA_2020ro_app_accept_county <- GA_2020ro_app_accept %>%
  count(County) %>%
  rename(Mail.Req.Tot = n)

GA_2020ro_app_accept_county_nhwhite <- GA_2020ro_app_accept %>%
  filter(RACE == "White not of Hispanic Origin") %>%
  count(County) %>%
  rename(Mail.Req.nhwhite.Tot = n)

GA_2020ro_app_accept_county_nhblack <- GA_2020ro_app_accept %>%
  filter(RACE == "Black not of Hispanic Origin") %>%
  count(County) %>%
  rename(Mail.Req.nhblack.Tot = n)

GA_2020ro_app_accept_county_nhasian <- GA_2020ro_app_accept %>%
  filter(RACE == "Asian or Pacific Islander") %>%
  count(County) %>%
  rename(Mail.Req.nhasian.Tot = n)

GA_2020ro_app_accept_county_nhna <- GA_2020ro_app_accept %>%
  filter(RACE == "American Indian or Alaskan Native") %>%
  count(County) %>%
  rename(Mail.Req.nhna.Tot = n)

GA_2020ro_app_accept_county_hisp <- GA_2020ro_app_accept %>%
  filter(RACE == "Hispanic") %>%
  count(County) %>%
  rename(Mail.Req.hisp.Tot = n)

GA_2020ro_app_accept_county_oth <- GA_2020ro_app_accept %>%
  filter(RACE != "Hispanic") %>%
  filter(RACE != "White not of Hispanic Origin") %>%
  filter(RACE != "Black not of Hispanic Origin") %>%
  filter(RACE != "Asian or Pacific Islander") %>%
  filter(RACE != "American Indian or Alaskan Native") %>%
  count(County) %>%
  rename(Mail.Req.oth.Tot = n)


GA_2020ro_race <- GA_2020ro_app_accept %>%
  count(RACE)

GA_2020ro_app_reject_county <- GA_2020ro_app_reject_unique %>%
  ungroup() %>%
  count(County) %>%
  rename(Mail.App.Reject.Tot = n)

GA_vr_county <- GA_vr_file %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters = n, County = COUNTY_NAME)
  
GA_county_data <- inner_join(GA_FIPS, GA_vr_county, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhwhite, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhblack, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhasian, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhna, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_hisp, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_oth, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_app_reject_county, by = "County")

GA_county_data <- GA_county_data %>% 
  mutate(Mail.Req.Tot = replace_na(Mail.Req.Tot, 0)) %>%
  mutate(Mail.App.Reject.Tot = replace_na(Mail.App.Reject.Tot, 0)) %>%
  mutate(Mail.Req.nhwhite.Tot = replace_na(Mail.Req.nhwhite.Tot, 0)) %>%
  mutate(Mail.Req.nhblack.Tot = replace_na(Mail.Req.nhblack.Tot, 0)) %>%
  mutate(Mail.Req.nhasian.Tot = replace_na(Mail.Req.nhasian.Tot, 0)) %>%
  mutate(Mail.Req.nhna.Tot = replace_na(Mail.Req.nhna.Tot, 0)) %>%
  mutate(Mail.Req.hisp.Tot = replace_na(Mail.Req.hisp.Tot, 0)) %>%
  mutate(Mail.Req.oth.Tot = replace_na(Mail.Req.oth.Tot, 0))

GA_county_data <- GA_county_data %>% 
  mutate(Pct.Req = Mail.Req.Tot/Reg.Voters) %>%
  mutate(Pct.App.Reject = Mail.App.Reject.Tot/(Mail.App.Reject.Tot + Mail.Req.Tot))

write_csv(GA_county_data, "D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/2020RO_Early_Vote_GA.csv")

