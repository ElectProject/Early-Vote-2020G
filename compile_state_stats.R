library(tidyverse)
library(readxl)
library(stringr)
library(scales)

###################
# Setup
###################

# Days before 2020 general election

election_day <- as.Date("2020-11-03")
today <- format(Sys.Date(), format = "%m/%d/%Y")

days_to_election <- election_day - today

date_before_2016g <- as.Date("2016-11-08")-days_to_election

days_until_ballots_2020 <- as.Date("2020-09-04") - today

###################
# Load summary statistics
###################

state_stats <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/2020G_Early_Vote.csv")

###################
# Florida
###################

url <- "http://fvrselectionfiles.elections.myflorida.com/countyballotreportfiles/Stats_10865_AbsProvided.txt"
destfile <- "D:/DropBox/Dropbox/Mail_Ballots_2020/FL/2020G/Stats_10865_AbsProvided.txt"

download.file(url, destfile, mode = "wb")

url <- "http://fvrselectionfiles.elections.myflorida.com/countyballotreportfiles/Stats_10865_AbsVoted.txt"
destfile <- "D:/DropBox/Dropbox/Mail_Ballots_2020/FL/2020G/Stats_10865_AbsVoted.txt"

download.file(url, destfile, mode = "wb")

FL_ab_2020g_sent <- read_tsv("D:/DropBox/Dropbox/Mail_Ballots_2020/FL/2020G/Stats_10865_AbsProvided.txt")

FL_ab_2020g_return <- read_tsv("D:/DropBox/Dropbox/Mail_Ballots_2020/FL/2020G/Stats_10865_AbsVoted.txt")

FL_ab_2020g_req_rep <- as.double(FL_ab_2020g_sent[1,6] + FL_ab_2020g_return[1,6])
FL_ab_2020g_req_dem <- as.double(FL_ab_2020g_sent[1,7] + FL_ab_2020g_return[1,7])
FL_ab_2020g_req_minor <- as.double(FL_ab_2020g_sent[1,8] + FL_ab_2020g_return[1,8])
FL_ab_2020g_req_npa <- as.double(FL_ab_2020g_sent[1,9] + FL_ab_2020g_return[1,9])
FL_ab_2020g_req_tot <- as.double(FL_ab_2020g_sent[1,10] + FL_ab_2020g_return[1,10])

state_stats <- state_stats %>%
  mutate(report_date = case_when(state == "FL" ~ as.character(today),
                                 TRUE ~ report_date)) %>%
  mutate(mail_sent_req_2020 = case_when(state == "FL" ~ FL_ab_2020g_req_tot,
                                        TRUE ~ mail_sent_req_2020)) %>%
  mutate(mail_sent_req_2020_dem = case_when(state == "FL" ~ FL_ab_2020g_req_dem,
                                        TRUE ~ mail_sent_req_2020_dem)) %>%
  mutate(mail_sent_req_2020_rep = case_when(state == "FL" ~ FL_ab_2020g_req_rep,
                                        TRUE ~ mail_sent_req_2020_rep)) %>%
  mutate(mail_sent_req_2020_npa = case_when(state == "FL" ~ FL_ab_2020g_req_npa,
                                        TRUE ~ mail_sent_req_2020_npa)) %>%
  mutate(mail_sent_req_2020_minor = case_when(state == "FL" ~ FL_ab_2020g_req_minor,
                                        TRUE ~ mail_sent_req_2020_minor))

###################
# Illinois
###################

IL_report_date <- "08/31/2020"

url <- "https://elections.il.gov/DocDisplay.aspx?Doc=Downloads/VotingAndRegistrationSystems/Counts/PreElection/Pre-election%20Ballot%20Requests.csv"
destfile <- "D:/DropBox/Dropbox/Mail_Ballots_2020/IL/2020G/Pre-election_Ballot_Requests.csv"

download.file(url, destfile, mode = "wb")

IL_2020g_stats <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/IL/2020G/Pre-election_Ballot_Requests.csv")

names(IL_2020g_stats)<-str_replace_all(names(IL_2020g_stats), c(" " = "."))
names(IL_2020g_stats)<-str_replace_all(names(IL_2020g_stats), c("-" = "."))

IL_2020g_stats_statewide <- IL_2020g_stats %>%
  filter(Name == "STATEWIDE COUNTS")

state_stats <- state_stats %>%
  mutate(report_date = case_when(state == "IL" ~ IL_report_date,
                                 TRUE ~ report_date)) %>%
  mutate(mail_sent_req_2020 = case_when(state == "IL" ~ IL_2020g_stats_statewide$By.Mail,
                                        TRUE ~ as.double(mail_sent_req_2020)))

###################
# Maine
###################

ME_report_date <- "09/1/2020"

url <- "https://www.maine.gov/sos/cec/elec/data/absentee-voter-file.txt"
destfile <- "D:/DropBox/Dropbox/Mail_Ballots_2020/ME/2020G/absentee-voter-file.txt"

download.file(url, destfile, mode = "wb")

ME_ab_2020g <- read_delim("D:/DropBox/Dropbox/Mail_Ballots_2020/ME/2020G/absentee-voter-file.txt", delim ="|")

ME_ab_2020g_party <- ME_ab_2020g %>%
  count(P)

state_stats <- state_stats %>%
  mutate(report_date = case_when(state == "ME" ~ ME_report_date,
                                 TRUE ~ report_date)) %>%
  mutate(mail_sent_req_2020 = case_when(state == "ME" ~ as.integer(nrow(ME_ab_2020g)),
                                        TRUE ~ as.integer(mail_sent_req_2020))) %>%
  mutate(mail_sent_req_2020_dem = case_when(state == "ME" ~ as.double(ME_ab_2020g_party[1,2]),
                                            TRUE ~ mail_sent_req_2020_dem)) %>%
  mutate(mail_sent_req_2020_rep = case_when(state == "ME" ~ as.double(ME_ab_2020g_party[3,2]),
                                            TRUE ~ mail_sent_req_2020_rep)) %>%
  mutate(mail_sent_req_2020_npa = case_when(state == "ME" ~ as.double(ME_ab_2020g_party[4,2]+ME_ab_2020g_party[5,2]),
                                            TRUE ~ mail_sent_req_2020_npa)) %>%
  mutate(mail_sent_req_2020_minor = case_when(state == "ME" ~ as.double(ME_ab_2020g_party[2,2]),
                                              TRUE ~ mail_sent_req_2020_minor))
  
###################
# North Carolina
###################

url <- "https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2020_11_03/absentee_demo_stats_20201103.csv"
destfile <- "D:/DropBox/Dropbox/Mail_Ballots_2020/NC/2020G/absentee_demo_stats_20201103.csv"

download.file(url, destfile, mode = "wb")

NC_2020g <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/NC/2020G/absentee_demo_stats_20201103.csv")

NC_2020g_ab_req_count_party <- NC_2020g %>%
  group_by(party_desc) %>%
  summarise(requests = sum(group_count))

NC_2020g_ab_req_count_nh_race <- NC_2020g %>%
  filter(ethncity_desc!="HISPANIC or LATINO") %>%
  group_by(race_desc) %>%
  summarise(requests = sum(group_count))

NC_2020g_ab_req_count_hisp <- NC_2020g %>%
  filter(ethncity_desc=="HISPANIC or LATINO") %>%
  group_by(ethncity_desc) %>%
  summarise(requests = sum(group_count))

NC_2020g_ab_req_count_gender <- NC_2020g %>%
  group_by(gender_desc) %>%
  summarise(requests = sum(group_count))

NC_2020g_ab_req_count_age <- NC_2020g %>%
  group_by(age_range) %>%
  summarise(requests = sum(group_count))

# Update the states_stats spreadsheet

state_stats <- state_stats %>%
  mutate(report_date = case_when(state == "NC" ~ as.character(today),
                                        TRUE ~ report_date)) %>%
  mutate(mail_sent_req_2020 = case_when(state == "NC" ~ sum(NC_2020g_ab_req_count_party$requests),
                                          TRUE ~ mail_sent_req_2020)) %>%
  mutate(mail_sent_req_2020_dem = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_party[2,2]),
                                          TRUE ~ mail_sent_req_2020_dem)) %>%
  mutate(mail_sent_req_2020_rep = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_party[5,2]),
                                          TRUE ~ mail_sent_req_2020_rep)) %>%
  mutate(mail_sent_req_2020_npa = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_party[6,2]),
                                          TRUE ~ mail_sent_req_2020_npa)) %>%
  mutate(mail_sent_req_2020_minor = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_party[1,2] + NC_2020g_ab_req_count_party[3,2] + NC_2020g_ab_req_count_party[4,2]),
                                          TRUE ~ mail_sent_req_2020_minor)) %>%
  mutate(mail_sent_req_2020_nh_white = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_nh_race[7,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_nh_white))) %>%
  mutate(mail_sent_req_2020_nh_black = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_nh_race[2,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_nh_black))) %>%
  mutate(mail_sent_req_2020_nh_asian = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_nh_race[1,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_nh_asian))) %>%
  mutate(mail_sent_req_2020_nh_native_american = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_nh_race[3,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_nh_native_american))) %>%
  mutate(mail_sent_req_2020_hispanic = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_hisp[1,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_hispanic))) %>%
  mutate(mail_sent_req_2020_other = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_nh_race[4,2]+NC_2020g_ab_req_count_nh_race[5,2]+NC_2020g_ab_req_count_nh_race[6,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_other))) %>% 
  mutate(mail_sent_req_2020_female = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_gender[1,2]),
                                            TRUE ~ as.double(mail_sent_req_2020_female))) %>%
  mutate(mail_sent_req_2020_male = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_gender[2,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_male))) %>%
  mutate(mail_sent_req_2020_unk = case_when(state == "NC" ~ as.double(NC_2020g_ab_req_count_gender[3,2]),
                                               TRUE ~ as.double(mail_sent_req_2020_unk)))

# NC county analysis

NC_vr <- read_tsv("D:/DropBox/Dropbox/Voter Files/NC/current/ncvoter_Statewide.txt", col_types = readcols)

NC_county_vr <- NC_vr %>%
  count(county_desc) %>%
  rename(Reg.Voters = n) %>%
  rename(County = county_desc)

NC_county_reqs <- NC_2020g %>%
  group_by(county_name) %>%
  summarise(Mail.Ballot.Requests = sum(group_count)) %>%
  rename(County = county_name)

NC_2020g_county_data <- left_join(NC_county_vr, NC_county_reqs, by = "County")

NC_2020g_county_data <- mutate(NC_2020g_county_data, Pct.Request = 100*Mail.Ballot.Requests/Reg.Voters)

write_csv(NC_2020g_county_data, "D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/2020G_Early_Vote_NC.csv")

###################
# Pennsylvania
###################

PA_ab_2020ppp <- read_csv("D:/DropBox/Dropbox/Mail_Ballots_2020/PA/2020_Primary_Election_Mail_Ballot_Requests_Department_of_State.csv")

names(PA_ab_2020ppp)<-str_replace_all(names(PA_ab_2020ppp), c(" " = "."))

# Party calculations

PA_ab_req_count <- PA_ab_2020ppp %>%
  count(Applicant.Party.Designation) %>%
  rename(requests = n)

PA_ab_req_count_dem <- PA_ab_req_count %>%
  filter(Applicant.Party.Designation == "D")

PA_ab_req_count_rep <- PA_ab_req_count %>%
  filter(Applicant.Party.Designation == "R")

PA_ab_req_count_npa <- PA_ab_req_count %>%
  filter(Applicant.Party.Designation == "NO"|Applicant.Party.Designation == "NON"|Applicant.Party.Designation == "NOP"|Applicant.Party.Designation == "NOPA"|Applicant.Party.Designation == "NF"|Applicant.Party.Designation == "NI"|Applicant.Party.Designation == "DTS"|Applicant.Party.Designation == "I"|Applicant.Party.Designation == "NF"|Applicant.Party.Designation == "NTS"|Applicant.Party.Designation == "UNA"|Applicant.Party.Designation == "UND"|Applicant.Party.Designation == "UNK"|is.na(Applicant.Party.Designation))

# Age calculations

PA_ab_2020ppp$BIRTH_YEAR <- str_sub(PA_ab_2020ppp$Date.of.Birth, -4, -1)

PA_ab_req_age1824 <- PA_ab_2020ppp %>%
  filter(BIRTH_YEAR>1995) %>%
  tally()

PA_ab_req_age2534 <- PA_ab_2020ppp %>%
  filter(BIRTH_YEAR>1985 & BIRTH_YEAR<1996) %>%
  tally()

PA_ab_req_age3544 <- PA_ab_2020ppp %>%
  filter(BIRTH_YEAR>1975 & BIRTH_YEAR<1986) %>%
  tally()

PA_ab_req_age4554 <- PA_ab_2020ppp %>%
  filter(BIRTH_YEAR>1965 & BIRTH_YEAR<1976) %>%
  tally()

PA_ab_req_age5564 <- PA_ab_2020ppp %>%
  filter(BIRTH_YEAR>1955 & BIRTH_YEAR<1966) %>%
  tally()

PA_ab_req_age65up <- PA_ab_2020ppp %>%
  filter(BIRTH_YEAR<1956) %>%
  tally()

# write to state stats

state_stats <- state_stats %>%
  mutate(mail_sent_req_2020 = case_when(state == "PA" ~ sum(PA_ab_req_count$requests),
                                            TRUE ~ as.integer(mail_sent_req_2020))) %>%
  mutate(mail_sent_req_2020_dem = case_when(state == "PA" ~ as.double(sum(PA_ab_req_count_dem$requests)),
                                            TRUE ~ as.double(mail_sent_req_2020_dem))) %>%
  mutate(mail_sent_req_2020_rep = case_when(state == "PA" ~ as.double(sum(PA_ab_req_count_rep$requests)),
                                            TRUE ~ as.double(mail_sent_req_2020_rep))) %>%
  mutate(mail_sent_req_2020_npa = case_when(state == "PA" ~ as.double(sum(PA_ab_req_count_npa$requests)),
                                            TRUE ~ as.double(mail_sent_req_2020_npa))) %>%
  mutate(mail_sent_req_2020_minor = case_when(state == "PA" ~ as.double(sum(PA_ab_req_count$requests) - sum(PA_ab_req_count_npa$requests) - sum(PA_ab_req_count_dem$requests) - sum(PA_ab_req_count_rep$requests)),
                                            TRUE ~ as.double(mail_sent_req_2020_minor))) %>%
  mutate(mail_sent_req_2020_age1824 = case_when(state == "PA" ~ as.double(PA_ab_req_age1824),
                                                TRUE ~ as.double(mail_sent_req_2020_age1824))) %>%
  mutate(mail_sent_req_2020_age2534 = case_when(state == "PA" ~ as.double(PA_ab_req_age2534),
                                                TRUE ~ as.double(mail_sent_req_2020_age2534))) %>%
  mutate(mail_sent_req_2020_age3544 = case_when(state == "PA" ~ as.double(PA_ab_req_age3544),
                                                TRUE ~ as.double(mail_sent_req_2020_age3544))) %>%
  mutate(mail_sent_req_2020_age4554 = case_when(state == "PA" ~ as.double(PA_ab_req_age4554),
                                                TRUE ~ as.double(mail_sent_req_2020_age4554))) %>%
  mutate(mail_sent_req_2020_age5564 = case_when(state == "PA" ~ as.double(PA_ab_req_age5564),
                                                TRUE ~ as.double(mail_sent_req_2020_age5564))) %>%
  mutate(mail_sent_req_2020_age65up = case_when(state == "PA" ~ as.double(PA_ab_req_age65up),
                                                TRUE ~ as.double(mail_sent_req_2020_age65up)))


# Using primary AB report. Add this back in once general election daily update is available
# mutate(report_date = case_when(state == "PA" ~ as.character(today),
#                               TRUE ~ report_date)) %>%
  

###################
# South Carolina
###################

SC_report_date <- "08/31/2020"
SC_file <- "D:/DropBox/Dropbox/Mail_Ballots_2020/SC/2020G/Absentee Stats 2020-08-31 (GE).xlsx"

# All

SC_2020g_stats_all <- read_xlsx(SC_file, sheet = 1, skip = 11)

names(SC_2020g_stats_all)<-str_replace_all(names(SC_2020g_stats_all), c(" " = "."))
names(SC_2020g_stats_all)<-str_replace_all(names(SC_2020g_stats_all), c("\r" = ""))
names(SC_2020g_stats_all)<-str_replace_all(names(SC_2020g_stats_all), c("\n" = ""))

SC_2020g_stats_all <- SC_2020g_stats_all %>%
  filter(County == "Total")

# NH White

SC_2020g_stats_nh_white <- read_xlsx(SC_file, sheet = 3, skip = 10)

names(SC_2020g_stats_nh_white)<-str_replace_all(names(SC_2020g_stats_nh_white), c(" " = "."))
names(SC_2020g_stats_nh_white)<-str_replace_all(names(SC_2020g_stats_nh_white), c("\r" = ""))
names(SC_2020g_stats_nh_white)<-str_replace_all(names(SC_2020g_stats_nh_white), c("\n" = ""))

SC_2020g_stats_nh_white <- SC_2020g_stats_nh_white %>%
  filter(County == "Total")

# NH Black
  
SC_2020g_stats_nh_black <- read_xlsx(SC_file, sheet = 2, skip = 10)

names(SC_2020g_stats_nh_black)<-str_replace_all(names(SC_2020g_stats_nh_black), c(" " = "."))
names(SC_2020g_stats_nh_black)<-str_replace_all(names(SC_2020g_stats_nh_black), c("\r" = ""))
names(SC_2020g_stats_nh_black)<-str_replace_all(names(SC_2020g_stats_nh_black), c("\n" = ""))

SC_2020g_stats_nh_black <- SC_2020g_stats_nh_black %>%
  filter(County == "Total")

# Hispanic

SC_2020g_stats_hispanic <- read_xlsx(SC_file, sheet = 5, skip = 10)

names(SC_2020g_stats_hispanic)<-str_replace_all(names(SC_2020g_stats_hispanic), c(" " = "."))
names(SC_2020g_stats_hispanic)<-str_replace_all(names(SC_2020g_stats_hispanic), c("\r" = ""))
names(SC_2020g_stats_hispanic)<-str_replace_all(names(SC_2020g_stats_hispanic), c("\n" = ""))

SC_2020g_stats_hispanic <- SC_2020g_stats_hispanic %>%
  filter(County == "Total")

# NH Asian

SC_2020g_stats_nh_asian <- read_xlsx(SC_file, sheet = 4, skip = 10)

names(SC_2020g_stats_nh_asian)<-str_replace_all(names(SC_2020g_stats_nh_asian), c(" " = "."))
names(SC_2020g_stats_nh_asian)<-str_replace_all(names(SC_2020g_stats_nh_asian), c("\r" = ""))
names(SC_2020g_stats_nh_asian)<-str_replace_all(names(SC_2020g_stats_nh_asian), c("\n" = ""))

SC_2020g_stats_nh_asian <- SC_2020g_stats_nh_asian %>%
  filter(County == "Total")

# NH Native American

SC_2020g_stats_nh_native_american <- read_xlsx(SC_file, sheet = 6, skip = 10)

names(SC_2020g_stats_nh_native_american)<-str_replace_all(names(SC_2020g_stats_nh_native_american), c(" " = "."))
names(SC_2020g_stats_nh_native_american)<-str_replace_all(names(SC_2020g_stats_nh_native_american), c("\r" = ""))
names(SC_2020g_stats_nh_native_american)<-str_replace_all(names(SC_2020g_stats_nh_native_american), c("\n" = ""))

SC_2020g_stats_nh_native_american <- SC_2020g_stats_nh_native_american %>%
  filter(County == "Total")

# Multiple

SC_2020g_stats_multiple <- read_xlsx(SC_file, sheet = 7, skip = 10)

names(SC_2020g_stats_multiple)<-str_replace_all(names(SC_2020g_stats_multiple), c(" " = "."))
names(SC_2020g_stats_multiple)<-str_replace_all(names(SC_2020g_stats_multiple), c("\r" = ""))
names(SC_2020g_stats_multiple)<-str_replace_all(names(SC_2020g_stats_multiple), c("\n" = ""))

SC_2020g_stats_multiple <- SC_2020g_stats_multiple %>%
  filter(County == "Total")

# Other

SC_2020g_stats_other <- read_xlsx(SC_file, sheet = 8, skip = 10)

names(SC_2020g_stats_other)<-str_replace_all(names(SC_2020g_stats_other), c(" " = "."))
names(SC_2020g_stats_other)<-str_replace_all(names(SC_2020g_stats_other), c("\r" = ""))
names(SC_2020g_stats_other)<-str_replace_all(names(SC_2020g_stats_other), c("\n" = ""))

SC_2020g_stats_other <- SC_2020g_stats_other %>%
  filter(County == "Total")

# Unknown

SC_2020g_stats_unknown <- read_xlsx(SC_file, sheet = 9, skip = 10)

names(SC_2020g_stats_unknown)<-str_replace_all(names(SC_2020g_stats_unknown), c(" " = "."))
names(SC_2020g_stats_unknown)<-str_replace_all(names(SC_2020g_stats_unknown), c("\r" = ""))
names(SC_2020g_stats_unknown)<-str_replace_all(names(SC_2020g_stats_unknown), c("\n" = ""))

SC_2020g_stats_unknown <- SC_2020g_stats_unknown %>%
  filter(County == "Total")

# Replace state_stats values

state_stats <- state_stats %>%
  mutate(report_date = case_when(state == "SC" ~ SC_report_date,
                                 TRUE ~ report_date)) %>%
  mutate(mail_sent_req_2020 = case_when(state == "SC" ~ as.double(SC_2020g_stats_all$Returned.Before.Deadline...5),
                                        TRUE ~ as.double(mail_sent_req_2020))) %>%
  mutate(mail_sent_req_2020_nh_white = case_when(state == "SC" ~ as.double(SC_2020g_stats_nh_white$Returned.Before.Deadline...5),
                                                 TRUE ~ as.double(mail_sent_req_2020_nh_white))) %>%
  mutate(mail_sent_req_2020_nh_black = case_when(state == "SC" ~ as.double(SC_2020g_stats_nh_black$Returned.Before.Deadline...5),
                                        TRUE ~ as.double(mail_sent_req_2020_nh_black))) %>%
  mutate(mail_sent_req_2020_nh_asian = case_when(state == "SC" ~ as.double(SC_2020g_stats_nh_asian$Returned.Before.Deadline...5),
                                                 TRUE ~ as.double(mail_sent_req_2020_nh_asian))) %>%
  mutate(mail_sent_req_2020_nh_native_american = case_when(state == "SC" ~ as.double(SC_2020g_stats_nh_native_american$Returned.Before.Deadline...5),
                                                 TRUE ~ as.double(mail_sent_req_2020_nh_native_american))) %>%
  mutate(mail_sent_req_2020_hispanic = case_when(state == "SC" ~ as.double(SC_2020g_stats_hispanic$Returned.Before.Deadline...5),
                                               TRUE ~ as.double(mail_sent_req_2020_hispanic))) %>%
  mutate(mail_sent_req_2020_other = case_when(state == "SC" ~ as.double(SC_2020g_stats_multiple$Returned.Before.Deadline...5 + SC_2020g_stats_other$Returned.Before.Deadline...5 + SC_2020g_stats_unknown$Returned.Before.Deadline...5),
                                                           TRUE ~ as.double(mail_sent_req_2020_other)))
  
###################
# Write state_stats
###################

write_csv(state_stats, "D:/DropBox/Dropbox/Mail_Ballots_2020/markdown/2020G_Early_Vote.csv")