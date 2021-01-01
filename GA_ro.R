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

GA_report_date <- "12/31/2020"

# Start here if already concatenated county files

GA_2020ro <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/GA/2020RO/STATEWIDE.csv")

names(GA_2020ro)<-str_replace_all(names(GA_2020ro), c(" " = "."))
names(GA_2020ro)<-str_replace_all(names(GA_2020ro), c("#" = ""))

GA_2020ro <- GA_2020ro %>%
  rename(REGISTRATION_NUMBER = Voter.Registration.)

# voter.file <- "D:/DropBox/Dropbox/Voter Files/GA/20201015/GA Voter List - Statewide - Alpha List.csv"
# readcolumns <- "ccccc_________cccc__c_________________________cccccc___________"
# readcolumns <- "ccccccccccccccccccccccccccccc"
# GA_vr_file <- read_csv(voter.file, col_types = readcolumns)

voter.file <- "D:/DropBox/Dropbox/Voter Files/GA/20201222/Georgia_Daily_VoterBase.txt"
readcolumns <- "ccccc_________cccc__c_________________________cccccc___________"
GA_vr_file <- read_delim(voter.file, delim = "|", quote = "", col_types = readcolumns)

GA_FIPS <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/GA_FIPS.csv", col_types = "ccc")

GA_vr_file <- left_join(GA_vr_file, GA_FIPS, by = "COUNTY_CODE")

GA_vr_file <- rename(GA_vr_file, COUNTY_NAME = County)

GA_race <- count(GA_vr_file, RACE)

# GA_vh_2016g <- read_fwf("D:/DropBox/Dropbox/Voter Files/GA/32668.TXT", fwf_widths(c(3,8,8,3,2,1,1,1)), col_types = "cccccccc")

GA_vh_2020g <- read_fwf("D:/DropBox/Dropbox/Mail_Ballots_2020/GA/2020G/35209.TXT", fwf_widths(c(3,8,8,3,2,1,1,1)), col_types = "cccccccc")

GA_vh_2020g <- GA_vh_2020g %>%
  mutate(voted_2020g = 1) %>%
  rename(REGISTRATION_NUMBER = X2) %>%
  select(REGISTRATION_NUMBER, voted_2020g)

GA_2020ro <- left_join(GA_2020ro, GA_vr_file, by = "REGISTRATION_NUMBER")
GA_2020ro <- left_join(GA_2020ro, GA_vh_2020g, by = "REGISTRATION_NUMBER")

GA_2020ro_app_accept <- GA_2020ro %>%
  filter(Ballot.Style == "MAILED"|Ballot.Style == "ELECTRONIC") %>%
  filter(Application.Status == "A")

GA_2020ro_app_reject <- GA_2020ro %>%
  filter(Ballot.Style == "MAILED"|Ballot.Style == "ELECTRONIC") %>%
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

write_csv(GA_2020ro_app_reject_unique, "D:/DropBox/Dropbox/Rejected_Ballots/GA_RO/GA_RO_Applications_Rejected_Statewide.csv")

GA_2020ro_app_reject_unique_ag <- GA_2020ro_app_reject_unique %>%
  filter(BIRTHDATE>1995)

write_csv(GA_2020ro_app_reject_unique_ag, "D:/DropBox/Dropbox/Rejected_Ballots/GA_RO/GA_RO_AG_Applications_Rejected_Statewide.csv")

GA_FIPS <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/GA_FIPS.csv", col_types = "cc")

GA_2020ro_app_accept_county <- GA_2020ro_app_accept %>%
  count(County) %>%
  rename(Mail.Req.Tot = n)

GA_2020ro_app_accept_county_nhwhite <- GA_2020ro_app_accept %>%
  filter(RACE == "WH") %>%
  count(County) %>%
  rename(Mail.Req.nhwhite.Tot = n)

GA_2020ro_app_accept_county_nhblack <- GA_2020ro_app_accept %>%
  filter(RACE == "BH") %>%
  count(County) %>%
  rename(Mail.Req.nhblack.Tot = n)

GA_2020ro_app_accept_county_nhasian <- GA_2020ro_app_accept %>%
  filter(RACE == "AP") %>%
  count(County) %>%
  rename(Mail.Req.nhasian.Tot = n)

GA_2020ro_app_accept_county_nhna <- GA_2020ro_app_accept %>%
  filter(RACE == "AI") %>%
  count(County) %>%
  rename(Mail.Req.nhna.Tot = n)

GA_2020ro_app_accept_county_hisp <- GA_2020ro_app_accept %>%
  filter(RACE == "HP") %>%
  count(County) %>%
  rename(Mail.Req.hisp.Tot = n)

GA_2020ro_app_accept_county_oth <- GA_2020ro_app_accept %>%
  filter(RACE != "HP") %>%
  filter(RACE != "WH") %>%
  filter(RACE != "BH") %>%
  filter(RACE != "AP") %>%
  filter(RACE != "AI") %>%
  count(County) %>%
  rename(Mail.Req.oth.Tot = n)

GA_2020ro_app_accept_county_voted <- GA_2020ro_app_accept %>%
  filter(!is.na(voted_2020g)) %>%
  count(County) %>%
  rename(Mail.Req.Vote.Tot = n)

GA_2020ro_app_accept_county_novoted <- GA_2020ro_app_accept %>%
  filter(is.na(voted_2020g)) %>%
  count(County) %>%
  rename(Mail.Req.Novote.Tot = n)

GA_2020ro_county_req_age1824 <- GA_2020ro_app_accept %>%
  filter(BIRTHDATE>1995) %>%
  count(County) %>%
  rename(Mail.Req.age1824.Tot = n)

GA_2020ro_county_req_age2534 <- GA_2020ro_app_accept %>%
  filter(BIRTHDATE>1985 & BIRTHDATE<1996) %>%
  count(County) %>%
  rename(Mail.Req.age2534.Tot = n)

GA_2020ro_county_req_age3544 <- GA_2020ro_app_accept %>%
  filter(BIRTHDATE>1975 & BIRTHDATE<1986) %>%
  count(County) %>%
  rename(Mail.Req.age3544.Tot = n)

GA_2020ro_county_req_age4554 <- GA_2020ro_app_accept %>%
  filter(BIRTHDATE>1965 & BIRTHDATE<1976) %>%
  count(County) %>%
  rename(Mail.Req.age4554.Tot = n)

GA_2020ro_county_req_age5564 <- GA_2020ro_app_accept %>%
  filter(BIRTHDATE>1955 & BIRTHDATE<1966) %>%
  count(County) %>%
  rename(Mail.Req.age5564.Tot = n)

GA_2020ro_county_req_age65up <- GA_2020ro_app_accept %>%
  filter(BIRTHDATE<1956) %>%
  count(County) %>%
  rename(Mail.Req.age65up.Tot = n)

GA_2020ro_county_req_ageunk <- GA_2020ro_app_accept %>%
  filter(is.na(BIRTHDATE)) %>%
  count(County) %>%
  rename(Mail.Req.ageunk.Tot = n)

## Application Rejections

GA_2020ro_app_reject_county <- GA_2020ro_app_reject_unique %>%
  ungroup() %>%
  count(County) %>%
  rename(Mail.App.Reject.Tot = n)

## Voter Registration

GA_vr_file <- left_join(GA_vr_file, GA_vh_2020g, by = "REGISTRATION_NUMBER")

GA_vr_county <- GA_vr_file %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters = n, County = COUNTY_NAME)

GA_vr_county_nhwhite <- GA_vr_file %>%
  filter(RACE == "WH") %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.nhwhite = n, County = COUNTY_NAME)

GA_vr_county_nhblack <- GA_vr_file %>%
  filter(RACE == "BH") %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.nhblack = n, County = COUNTY_NAME)

GA_vr_county_nhasian <- GA_vr_file %>%
  filter(RACE == "AP") %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.nhasian = n, County = COUNTY_NAME)

GA_vr_county_nhna <- GA_vr_file %>%
  filter(RACE == "AI") %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.nhna = n, County = COUNTY_NAME)

GA_vr_county_hisp <- GA_vr_file %>%
  filter(RACE == "HP") %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.hisp = n, County = COUNTY_NAME)

GA_vr_county_oth <- GA_vr_file %>%
  filter(RACE != "HP") %>%
  filter(RACE != "WH") %>%
  filter(RACE != "BH") %>%
  filter(RACE != "AP") %>%
  filter(RACE != "AI") %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.oth = n, County = COUNTY_NAME)

GA_vr_county_voted <- GA_vr_file %>%
  filter(!is.na(voted_2020g)) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.Vote = n, County = COUNTY_NAME)

GA_vr_county_novoted <- GA_vr_file %>%
  filter(is.na(voted_2020g)) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.Novote = n, County = COUNTY_NAME)

GA_vr_county_age1824 <- GA_vr_file %>%
  filter(BIRTHDATE>1995) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.age1824 = n, County = COUNTY_NAME)

GA_vr_county_age2534 <- GA_vr_file %>%
  filter(BIRTHDATE>1985 & BIRTHDATE<1996) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.age2534 = n, County = COUNTY_NAME)

GA_vr_county_age3544 <- GA_vr_file %>%
  filter(BIRTHDATE>1975 & BIRTHDATE<1986) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.age3544 = n, County = COUNTY_NAME)

GA_vr_county_age4554 <- GA_vr_file %>%
  filter(BIRTHDATE>1965 & BIRTHDATE<1976) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.age4554 = n, County = COUNTY_NAME)

GA_vr_county_age5564 <- GA_vr_file %>%
  filter(BIRTHDATE>1955 & BIRTHDATE<1966) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.age5564 = n, County = COUNTY_NAME)

GA_vr_county_age65up <- GA_vr_file %>%
  filter(BIRTHDATE<1956) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.age65up = n, County = COUNTY_NAME)

GA_vr_county_ageunk <- GA_vr_file %>%
  filter(is.na(BIRTHDATE)) %>%
  count(COUNTY_NAME) %>%
  rename(Reg.Voters.ageunk = n, County = COUNTY_NAME)

## Accepted Mail Ballots

GA_2020ro_mail_accept <- GA_2020ro %>%
  filter(Ballot.Style == "MAILED"|Ballot.Style == "ELECTRONIC") %>%
  filter(Ballot.Status == "A")

GA_2020ro_mail_accept_county <- GA_2020ro_mail_accept %>%
  count(County) %>%
  rename(Mail.Accept.Tot = n)

GA_2020ro_mail_accept_county_nhwhite <- GA_2020ro_mail_accept %>%
  filter(RACE == "WH") %>%
  count(County) %>%
  rename(Mail.Accept.nhwhite.Tot = n)

GA_2020ro_mail_accept_county_nhblack <- GA_2020ro_mail_accept %>%
  filter(RACE == "BH") %>%
  count(County) %>%
  rename(Mail.Accept.nhblack.Tot = n)

GA_2020ro_mail_accept_county_nhasian <- GA_2020ro_mail_accept %>%
  filter(RACE == "AP") %>%
  count(County) %>%
  rename(Mail.Accept.nhasian.Tot = n)

GA_2020ro_mail_accept_county_nhna <- GA_2020ro_mail_accept %>%
  filter(RACE == "AI") %>%
  count(County) %>%
  rename(Mail.Accept.nhna.Tot = n)

GA_2020ro_mail_accept_county_hisp <- GA_2020ro_mail_accept %>%
  filter(RACE == "HP") %>%
  count(County) %>%
  rename(Mail.Accept.hisp.Tot = n)

GA_2020ro_mail_accept_county_oth <- GA_2020ro_mail_accept %>%
  filter(RACE != "HP") %>%
  filter(RACE != "WH") %>%
  filter(RACE != "BH") %>%
  filter(RACE != "AP") %>%
  filter(RACE != "AI") %>%
  count(County) %>%
  rename(Mail.Accept.oth.Tot = n)

GA_2020ro_mail_accept_county_voted <- GA_2020ro_mail_accept %>%
  filter(!is.na(voted_2020g)) %>%
  count(County) %>%
  rename(Mail.Accept.Vote.Tot = n)

GA_2020ro_mail_accept_county_novoted <- GA_2020ro_mail_accept %>%
  filter(is.na(voted_2020g)) %>%
  count(County) %>%
  rename(Mail.Accept.Novote.Tot = n)

GA_2020ro_mail_accept_county_age1824 <- GA_2020ro_mail_accept %>%
  filter(BIRTHDATE>1995) %>%
  count(County) %>%
  rename(Mail.Accept.age1824.Tot = n)

GA_2020ro_mail_accept_county_age2534 <- GA_2020ro_mail_accept %>%
  filter(BIRTHDATE>1985 & BIRTHDATE<1996) %>%
  count(County) %>%
  rename(Mail.Accept.age2534.Tot = n)

GA_2020ro_mail_accept_county_age3544 <- GA_2020ro_mail_accept %>%
  filter(BIRTHDATE>1975 & BIRTHDATE<1986) %>%
  count(County) %>%
  rename(Mail.Accept.age3544.Tot = n)

GA_2020ro_mail_accept_county_age4554 <- GA_2020ro_mail_accept %>%
  filter(BIRTHDATE>1965 & BIRTHDATE<1976) %>%
  count(County) %>%
  rename(Mail.Accept.age4554.Tot = n)

GA_2020ro_mail_accept_county_age5564 <- GA_2020ro_mail_accept %>%
  filter(BIRTHDATE>1955 & BIRTHDATE<1966) %>%
  count(County) %>%
  rename(Mail.Accept.age5564.Tot = n)

GA_2020ro_mail_accept_county_age65up <- GA_2020ro_mail_accept %>%
  filter(BIRTHDATE<1956) %>%
  count(County) %>%
  rename(Mail.Accept.age65up.Tot = n)

GA_2020ro_mail_accept_county_ageunk <- GA_2020ro_mail_accept %>%
  filter(is.na(BIRTHDATE)) %>%
  count(County) %>%
  rename(Mail.Accept.ageunk.Tot = n)

# In-person

GA_2020ro_inperson_accept <- GA_2020ro %>%
  filter(Ballot.Style == "IN PERSON") %>%
  filter(Ballot.Status == "A")

GA_2020ro_inperson_accept_county <- GA_2020ro_inperson_accept %>%
  count(County) %>%
  rename(Inperson.Accept.Tot = n)

GA_2020ro_inperson_accept_county_nhwhite <- GA_2020ro_inperson_accept %>%
  filter(RACE == "WH") %>%
  count(County) %>%
  rename(Inperson.Accept.nhwhite.Tot = n)

GA_2020ro_inperson_accept_county_nhblack <- GA_2020ro_inperson_accept %>%
  filter(RACE == "BH") %>%
  count(County) %>%
  rename(Inperson.Accept.nhblack.Tot = n)

GA_2020ro_inperson_accept_county_nhasian <- GA_2020ro_inperson_accept %>%
  filter(RACE == "AP") %>%
  count(County) %>%
  rename(Inperson.Accept.nhasian.Tot = n)

GA_2020ro_inperson_accept_county_nhna <- GA_2020ro_inperson_accept %>%
  filter(RACE == "AI") %>%
  count(County) %>%
  rename(Inperson.Accept.nhna.Tot = n)

GA_2020ro_inperson_accept_county_hisp <- GA_2020ro_inperson_accept %>%
  filter(RACE == "HP") %>%
  count(County) %>%
  rename(Inperson.Accept.hisp.Tot = n)

GA_2020ro_inperson_accept_county_oth <- GA_2020ro_inperson_accept %>%
  filter(RACE != "HP") %>%
  filter(RACE != "WH") %>%
  filter(RACE != "BH") %>%
  filter(RACE != "AP") %>%
  filter(RACE != "AI") %>%
  count(County) %>%
  rename(Inperson.Accept.oth.Tot = n)

GA_2020ro_inperson_accept_county_voted <- GA_2020ro_inperson_accept %>%
  filter(!is.na(voted_2020g)) %>%
  count(County) %>%
  rename(Inperson.Accept.Vote.Tot = n)

GA_2020ro_inperson_accept_county_novoted <- GA_2020ro_inperson_accept %>%
  filter(is.na(voted_2020g)) %>%
  count(County) %>%
  rename(Inperson.Accept.Novote.Tot = n)

GA_2020ro_inperson_accept_county_age1824 <- GA_2020ro_inperson_accept %>%
  filter(BIRTHDATE>1995) %>%
  count(County) %>%
  rename(Inperson.Accept.age1824.Tot = n)

GA_2020ro_inperson_accept_county_age2534 <- GA_2020ro_inperson_accept %>%
  filter(BIRTHDATE>1985 & BIRTHDATE<1996) %>%
  count(County) %>%
  rename(Inperson.Accept.age2534.Tot = n)

GA_2020ro_inperson_accept_county_age3544 <- GA_2020ro_inperson_accept %>%
  filter(BIRTHDATE>1975 & BIRTHDATE<1986) %>%
  count(County) %>%
  rename(Inperson.Accept.age3544.Tot = n)

GA_2020ro_inperson_accept_county_age4554 <- GA_2020ro_inperson_accept %>%
  filter(BIRTHDATE>1965 & BIRTHDATE<1976) %>%
  count(County) %>%
  rename(Inperson.Accept.age4554.Tot = n)

GA_2020ro_inperson_accept_county_age5564 <- GA_2020ro_inperson_accept %>%
  filter(BIRTHDATE>1955 & BIRTHDATE<1966) %>%
  count(County) %>%
  rename(Inperson.Accept.age5564.Tot = n)

GA_2020ro_inperson_accept_county_age65up <- GA_2020ro_inperson_accept %>%
  filter(BIRTHDATE<1956) %>%
  count(County) %>%
  rename(Inperson.Accept.age65up.Tot = n)

GA_2020ro_inperson_accept_county_ageunk <- GA_2020ro_inperson_accept %>%
  filter(is.na(BIRTHDATE)) %>%
  count(County) %>%
  rename(Inperson.Accept.ageunk.Tot = n)


## Rejected ballots

GA_2020ro_mail_reject <- GA_2020ro %>%
  filter(Ballot.Style == "MAILED"|Ballot.Style == "ELECTRONIC") %>%
  filter(Ballot.Status == "R")

# de-dupe

GA_2020ro_mail_reject_most_recent <- GA_2020ro_mail_reject %>%
  arrange(REGISTRATION_NUMBER, Ballot.Return.Date) %>%
  group_by(REGISTRATION_NUMBER) %>%
  summarise(recent = last(Ballot.Return.Date), applications=n()) %>%
  mutate(mail_recent = "Y") %>%
  rename(Ballot.Return.Date = recent) %>%
  mutate(date_added = GA_report_date)

GA_2020ro_mail_reject_unique <- left_join(GA_2020ro_mail_reject, GA_2020ro_mail_reject_most_recent, by = c("REGISTRATION_NUMBER", "Ballot.Return.Date"))

GA_2020ro_mail_reject_unique <- GA_2020ro_mail_reject_unique %>%
  filter(!is.na(mail_recent))

# Remove rejected ballots for registrants who voted

GA_2020ro_voted <- GA_2020ro %>%
  filter(Ballot.Status == "A") %>%
  select(REGISTRATION_NUMBER) %>%
  mutate(voted = "Y")

GA_2020ro_mail_reject_unique <- left_join(GA_2020ro_mail_reject_unique, GA_2020ro_voted, by = "REGISTRATION_NUMBER") 

GA_2020ro_mail_reject_unique <- GA_2020ro_mail_reject_unique %>%
  filter(is.na(voted))

# merge in prior file

GA_2020ro_mail_reject_prior <- read_csv("D:/DropBox/Dropbox/Rejected_Ballots/GA_RO/GA_RO_Mail_Rejected_Statewide.csv")

GA_2020ro_mail_reject_prior <- GA_2020ro_mail_reject_prior %>%
  select(-cured)

GA_2020ro_mail_reject_prior_list <- GA_2020ro_mail_reject_prior %>%
  select(REGISTRATION_NUMBER) %>%
  mutate(prior = "Y")

GA_2020ro_mail_reject_unique_new <- left_join(GA_2020ro_mail_reject_unique, GA_2020ro_mail_reject_prior_list, by = "REGISTRATION_NUMBER")

GA_2020ro_mail_reject_unique_new <- GA_2020ro_mail_reject_unique_new %>%
  filter(is.na(prior)) %>%
  select(-prior) %>%
  select(-voted) 

GA_2020ro_mail_reject_dated <- rbind(GA_2020ro_mail_reject_prior, GA_2020ro_mail_reject_unique_new)

# add if prior ballot rejection has been cured
# IMPORTANT: in future need to change since rbind won't work (columns unbalanced)

GA_2020ro_voted <- GA_2020ro_voted %>%
  rename(cured = voted)

GA_2020ro_mail_reject_dated <- left_join(GA_2020ro_mail_reject_dated, GA_2020ro_voted, by = "REGISTRATION_NUMBER")

# write files

write_csv(GA_2020ro_mail_reject_dated, "D:/DropBox/Dropbox/Rejected_Ballots/GA_RO/GA_RO_Mail_Rejected_Statewide.csv")

GA_2020ro_mail_reject_dated_ag <- GA_2020ro_mail_reject_dated %>%
  filter(BIRTHDATE>1995)

write_csv(GA_2020ro_mail_reject_dated_ag, "D:/DropBox/Dropbox/Rejected_Ballots/GA_RO/GA_RO_AG_Mail_Rejected_Statewide.csv")

GA_2020ro_mail_reject_county <- GA_2020ro_mail_reject_unique %>%
  count(County) %>%
  rename(Mail.Reject.Tot = n)

# Build County Database

GA_county_data <- inner_join(GA_FIPS, GA_vr_county, by = "County")

GA_county_data <- left_join(GA_county_data, GA_vr_county_nhwhite, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_nhblack, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_nhasian, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_nhna, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_hisp, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_oth, by = "County")

GA_county_data <- left_join(GA_county_data, GA_vr_county_voted, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_novoted, by = "County")

GA_county_data <- left_join(GA_county_data, GA_vr_county_age1824, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_age2534, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_age3544, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_age4554, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_age5564, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_age65up, by = "County")
GA_county_data <- left_join(GA_county_data, GA_vr_county_ageunk, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_nhwhite, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_nhblack, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_nhasian, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_nhna, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_hisp, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_oth, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_voted, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_novoted, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_age1824, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_age2534, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_age3544, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_age4554, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_age5564, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_age65up, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_accept_county_ageunk, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_nhwhite, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_nhblack, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_nhasian, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_nhna, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_hisp, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_oth, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_voted, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_novoted, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_age1824, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_age2534, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_age3544, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_age4554, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_age5564, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_age65up, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_inperson_accept_county_ageunk, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhwhite, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhblack, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhasian, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_nhna, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_hisp, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_oth, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_voted, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_app_accept_county_novoted, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_county_req_age1824, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_county_req_age2534, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_county_req_age3544, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_county_req_age4554, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_county_req_age5564, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_county_req_age65up, by = "County")
GA_county_data <- left_join(GA_county_data, GA_2020ro_county_req_ageunk, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_app_reject_county, by = "County")

GA_county_data <- left_join(GA_county_data, GA_2020ro_mail_reject_county, by = "County")

GA_county_data <- GA_county_data %>% 
  mutate(Mail.Req.Tot = replace_na(Mail.Req.Tot, 0)) %>%
  mutate(Mail.App.Reject.Tot = replace_na(Mail.App.Reject.Tot, 0)) %>%
  mutate(Mail.Req.nhwhite.Tot = replace_na(Mail.Req.nhwhite.Tot, 0)) %>%
  mutate(Mail.Req.nhblack.Tot = replace_na(Mail.Req.nhblack.Tot, 0)) %>%
  mutate(Mail.Req.nhasian.Tot = replace_na(Mail.Req.nhasian.Tot, 0)) %>%
  mutate(Mail.Req.nhna.Tot = replace_na(Mail.Req.nhna.Tot, 0)) %>%
  mutate(Mail.Req.hisp.Tot = replace_na(Mail.Req.hisp.Tot, 0)) %>%
  mutate(Mail.Req.oth.Tot = replace_na(Mail.Req.oth.Tot, 0)) %>%
  mutate(Mail.Req.Vote.Tot = replace_na(Mail.Req.Vote.Tot, 0)) %>%
  mutate(Mail.Req.Novote.Tot = replace_na(Mail.Req.Novote.Tot, 0)) %>%
  mutate(Mail.Req.age1824.Tot = replace_na(Mail.Req.age1824.Tot, 0)) %>%
  mutate(Mail.Req.age2534.Tot = replace_na(Mail.Req.age2534.Tot, 0)) %>%
  mutate(Mail.Req.age3544.Tot = replace_na(Mail.Req.age3544.Tot, 0)) %>%
  mutate(Mail.Req.age4554.Tot = replace_na(Mail.Req.age4554.Tot, 0)) %>%
  mutate(Mail.Req.age5564.Tot = replace_na(Mail.Req.age5564.Tot, 0)) %>%
  mutate(Mail.Req.age65up.Tot = replace_na(Mail.Req.age65up.Tot, 0)) %>%
  mutate(Mail.Req.ageunk.Tot = replace_na(Mail.Req.ageunk.Tot, 0)) %>%
  mutate(Mail.Accept.Tot = replace_na(Mail.Accept.Tot, 0)) %>%
  mutate(Mail.Accept.nhwhite.Tot = replace_na(Mail.Accept.nhwhite.Tot, 0)) %>%
  mutate(Mail.Accept.nhblack.Tot = replace_na(Mail.Accept.nhblack.Tot, 0)) %>%
  mutate(Mail.Accept.nhasian.Tot = replace_na(Mail.Accept.nhasian.Tot, 0)) %>%
  mutate(Mail.Accept.nhna.Tot = replace_na(Mail.Accept.nhna.Tot, 0)) %>%
  mutate(Mail.Accept.hisp.Tot = replace_na(Mail.Accept.hisp.Tot, 0)) %>%
  mutate(Mail.Accept.oth.Tot = replace_na(Mail.Accept.oth.Tot, 0)) %>%
  mutate(Mail.Accept.Vote.Tot = replace_na(Mail.Accept.Vote.Tot, 0)) %>%
  mutate(Mail.Accept.Novote.Tot = replace_na(Mail.Accept.Novote.Tot, 0)) %>%
  mutate(Mail.Accept.age1824.Tot = replace_na(Mail.Accept.age1824.Tot, 0)) %>%
  mutate(Mail.Accept.age2534.Tot = replace_na(Mail.Accept.age2534.Tot, 0)) %>%
  mutate(Mail.Accept.age3544.Tot = replace_na(Mail.Accept.age3544.Tot, 0)) %>%
  mutate(Mail.Accept.age4554.Tot = replace_na(Mail.Accept.age4554.Tot, 0)) %>%
  mutate(Mail.Accept.age5564.Tot = replace_na(Mail.Accept.age5564.Tot, 0)) %>%
  mutate(Mail.Accept.age65up.Tot = replace_na(Mail.Accept.age65up.Tot, 0)) %>%
  mutate(Mail.Accept.ageunk.Tot = replace_na(Mail.Accept.ageunk.Tot, 0)) %>%
  mutate(Inperson.Accept.Tot = replace_na(Inperson.Accept.Tot, 0)) %>%
  mutate(Inperson.Accept.nhwhite.Tot = replace_na(Inperson.Accept.nhwhite.Tot, 0)) %>%
  mutate(Inperson.Accept.nhblack.Tot = replace_na(Inperson.Accept.nhblack.Tot, 0)) %>%
  mutate(Inperson.Accept.nhasian.Tot = replace_na(Inperson.Accept.nhasian.Tot, 0)) %>%
  mutate(Inperson.Accept.nhna.Tot = replace_na(Inperson.Accept.nhna.Tot, 0)) %>%
  mutate(Inperson.Accept.hisp.Tot = replace_na(Inperson.Accept.hisp.Tot, 0)) %>%
  mutate(Inperson.Accept.oth.Tot = replace_na(Inperson.Accept.oth.Tot, 0)) %>%
  mutate(Inperson.Accept.Vote.Tot = replace_na(Inperson.Accept.Vote.Tot, 0)) %>%
  mutate(Inperson.Accept.Novote.Tot = replace_na(Inperson.Accept.Novote.Tot, 0)) %>%
  mutate(Inperson.Accept.age1824.Tot = replace_na(Inperson.Accept.age1824.Tot, 0)) %>%
  mutate(Inperson.Accept.age2534.Tot = replace_na(Inperson.Accept.age2534.Tot, 0)) %>%
  mutate(Inperson.Accept.age3544.Tot = replace_na(Inperson.Accept.age3544.Tot, 0)) %>%
  mutate(Inperson.Accept.age4554.Tot = replace_na(Inperson.Accept.age4554.Tot, 0)) %>%
  mutate(Inperson.Accept.age5564.Tot = replace_na(Inperson.Accept.age5564.Tot, 0)) %>%
  mutate(Inperson.Accept.age65up.Tot = replace_na(Inperson.Accept.age65up.Tot, 0)) %>%
  mutate(Inperson.Accept.ageunk.Tot = replace_na(Inperson.Accept.ageunk.Tot, 0)) %>%
  mutate(Reg.Voters.nhwhite = replace_na(Reg.Voters.nhwhite, 0)) %>%
  mutate(Reg.Voters.nhblack = replace_na(Reg.Voters.nhblack, 0)) %>%
  mutate(Reg.Voters.nhasian = replace_na(Reg.Voters.nhasian, 0)) %>%
  mutate(Reg.Voters.nhna = replace_na(Reg.Voters.nhna, 0)) %>%
  mutate(Reg.Voters.hisp = replace_na(Reg.Voters.hisp, 0)) %>%
  mutate(Reg.Voters.oth = replace_na(Reg.Voters.oth, 0)) %>%
  mutate(Reg.Voters.Vote = replace_na(Reg.Voters.Vote, 0)) %>%
  mutate(Reg.Voters.Novote = replace_na(Reg.Voters.Novote, 0)) %>%
  mutate(Reg.Voters.age1824 = replace_na(Reg.Voters.age1824, 0)) %>%
  mutate(Reg.Voters.age2534 = replace_na(Reg.Voters.age2534, 0)) %>%
  mutate(Reg.Voters.age3544 = replace_na(Reg.Voters.age3544, 0)) %>%
  mutate(Reg.Voters.age4554 = replace_na(Reg.Voters.age4554, 0)) %>%
  mutate(Reg.Voters.age5564 = replace_na(Reg.Voters.age5564, 0)) %>%
  mutate(Reg.Voters.age65up = replace_na(Reg.Voters.age65up, 0)) %>%
  mutate(Reg.Voters.ageunk = replace_na(Reg.Voters.ageunk, 0)) %>%
  mutate(Mail.Reject.Tot = replace_na(Mail.Reject.Tot, 0))
  
GA_county_data <- GA_county_data %>% 
  mutate(Pct.Voted = (Mail.Accept.Tot + Inperson.Accept.Tot)/Reg.Voters) %>%
  mutate(Pct.Inperson = (Inperson.Accept.Tot/Reg.Voters)) %>%
  mutate(Pct.Mail.Accept = Mail.Accept.Tot/Mail.Req.Tot) %>%
  mutate(Pct.Req = Mail.Req.Tot/Reg.Voters) %>%
  mutate(Pct.App.Reject = Mail.App.Reject.Tot/(Mail.App.Reject.Tot + Mail.Req.Tot)) %>%
  mutate(Pct.Mail.Reject = Mail.Reject.Tot/(Mail.Reject.Tot + Mail.Accept.Tot))

write_csv(GA_county_data, "D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/2020RO_Early_Vote_GA.csv")

