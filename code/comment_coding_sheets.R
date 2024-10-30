
source("setup.R")

done <- list.files(here::here("data", "datasheets")) %>%
  str_remove("_.*")

library(DBI)
library(RSQLite)
library(tidyverse)

#######################################
# Database location on linux
master_location <- here::here("db", "comment_metadata.sqlite")
master_con = dbConnect(SQLite(), dbname=master_location)
dbListTables(master_con)

#######################################
# Database location mac
master_location <- here::here("data", "rulemaking_metadata.sqlite")
master_con = dbConnect(SQLite(), dbname=master_location)
dbListTables(master_con)

rules <- dbSendQuery(master_con, 'SELECT * FROM rules')


load(here::here("data", "rules_metadata.Rdata"))
rules %<>% as_tibble()
names(rules)
dim(rules)

rules$posted_date %<>% as.Date()
max(rules$posted_date, na.rm = T)
min(rules$posted_date, na.rm = T)

rules %<>% mutate(year = str_sub(posted_date, 0, 4) %>% as.integer())
unique(rules$year)

rules %<>% filter(year > 2008, year < 2021)

# inspect for completeness (metadata are missing notices for 2018 and 2019)
count(rules, year)
rules %>% filter(document_type =="Rule") %>% count(year)
rules %>% filter(document_type =="Proposed Rule") %>% count(year)

#FIXME
# Sampling

if(ej_sample){
load(here::here("data", "ejdockets.Rdata"))
head(ejdockets)
rules %<>% filter(docket_id %in% ejdockets$docket_id)
}

if(mass_sample){
  rules %<>% filter(number_of_comments_received  > 99)
}


nrow(rules)


# load(here::here("data", "ejdockets.Rdata"))
# head(ejdockets)
# rules %<>% filter(docket_id %in% ejdockets$docket_id)
nrow(rules)

done <- list.files(here::here("data", "datasheets")) %>%
  str_remove("_.*")

# rules %<>% filter(docket_id %in% done)
nrow(rules)
#/FIXME


# comments
# load(here::here("data", "comment_metadata.rdata"))
names(comments_all)
# load(here::here("data", "comment_metadata_2020.Rdata"))
names(comment_metadata)



call <- 'SELECT * FROM comments WHERE docket_id = "EPA-HQ-OAR-2008-0699"'
call <- 'SELECT * FROM comments WHERE docket_id = "FNS-2011-0019"'
call <- 'SELECT * FROM comments WHERE docket_id = "WHD-2017-0003"'

dockets <-   rules %<>% filter(number_of_comments_received  > 99) %>% pull(docket_id)

call <- paste('SELECT * FROM comments WHERE docket_id IN', dockets)


comment_metadata <-  dbSendQuery(master_con, call) %>% dbFetch(Master_Query, n = -1)

# comments
load(here::here("data", "comment_metadata_2020.Rdata"))
# Rename if loading from old comment metadata (but this will cause other problems)
# comment_metadata <- comments_all


dim(comment_metadata)
ls()
names(comment_metadata)
# names(comments_all)

d <- comment_metadata
d$posted_date %<>% as.Date()
max(d$posted_date, na.rm = T)
min(d$posted_date, na.rm = T)


if(sample){
topdockets <- rules %>%
  group_by(docket_id) %>%


topdockets <- rules %>%
  group_by(docket_id) %>%
  mutate(number_of_comments_received = sum(number_of_comments_received)) %>%
  ungroup() %>%
  filter(docket_type == "Rulemaking",
         number_of_comments_received > 99, # dockets with 100 comments
         document_type == "Rule") %>% # dockets with a final rule (initially sampled all dockets)
  # drop ones with more than one FR
  add_count(docket_id) %>% filter(n == 1) %>%
  group_by(agency_id) %>%
  # agencie that have mass dockets
  mutate(max_comments = max(number_of_comments_received)) %>%
  #filter(max_comments > 100) %>%
  # SAMPLE
  slice_sample(weight_by = number_of_comments_received,
            n = 5)
}

nrow(topdockets)


topdockets$number_of_comments_received %>% head()
head(topdockets$docket_id)
dim(topdockets)

count(topdockets, docket_id, number_of_comments_received)

d %<>%
  # filter to top dockets
  filter(docket_id %in% topdockets$docket_id)

agencies <- unique(rules$agency_id)

agencies = c(#"EPA",
             "ATF", "BIA", "HUD"
#              "NLRB",
#              "OFCCP",
#              "OJP",
#              "USCG",
#              "CIS",
#              "USCBP",
#              "PHMSA",
#              "DOS",
#              "ED",
#              "MSHA",
#              "BSEE",
#              "DOJ-CRT",
#              "DOL",
#              "BIA",
#              "FEMA",
#              "BLM",
#              "DOI",
#              "DEA",
#              "OSHA",
#              "DARS",
#              "DHS"
)

d %<>%
  # filter to top dockets
  filter(docket_id %in% topdockets$docket_id)

d %<>%
  # selecting agencies for hand codeing
  filter(agency_id %in% agencies)

d %<>% filter(docket_type == "Rulemaking")

dim(d)

}

# selecting agencies for hand codeing
agencies <- unique(rules$agency_id)


d %<>% mutate(agency_acronym = agency_id)
d %<>% mutate(document_id = id)


d %<>%
  filter(agency_acronym %in% agencies)

# filter to mass dockets
d %<>% group_by(docket_id) %>%
  # mass dockets
  mutate(comments_on_docket = as.numeric(number_of_comments_received) %>% sum(),
         max = max(number_of_comments_received) ) %>%
  ungroup() %>%
  filter(max > 99 | comments_on_docket > 999)
dim(d)

names(d)


#FIXME with updated org_names from hand-coding

d %<>% filter(attachment_count > 0,
       !str_detect(organization, "^.\\. |illegible|surname|last name|forename|no name|^unknown$"),
       !str_detect(title, "illegible|surname|last name|forename|no name") )
dim(d)

d %<>% mutate(agency_acronym = agency_id)
d %<>% mutate(document_id = id)

# apply auto-coding
#FIXME with updated org_names from hand-coding


source(here::here("code", "org_name.R"))

#FIXME source(here::here("code", "comment_position.R"))

save(d, file = here::here("data", "comments4datasheets.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))
dim(d)
temp <- d
d <- temp

d %>% count(org_name, sort = T)

d %<>% mutate(org_name = ifelse(str_dct(title, "Chief,"),
                               str_remove(title, ".*Chief,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Member of Congress|Senat|Rep\\.|Sen\\.|House of Representatives"),
                               title,
                               org_name),
             org_name = ifelse(str_dct(title, ", Counsel,"),
                               str_remove(title, ".*, Counsel,"),
                               org_name),
             org_name = ifelse(str_dct(title, "President,"),
                               str_remove(title, ".*President,"),
                               org_name),
             org_name = ifelse(str_dct(title, ", Chairman,"),
                               str_remove(title, ".*, Chairman,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Chair,"),
                               str_remove(title, ".*Chair, "),
                               org_name),
             org_name = ifelse(str_dct(title, ", MPA,"),
                               str_remove(title, ".*, MPA,"),
                               org_name),
             org_name = ifelse(str_dct(title, ", Council Chair,"),
                               str_remove(title, ".*, Council Chair,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Director,"),
                               str_remove(title, ".*Director,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Commissioner,"),
                               str_remove(title, ".*Commissioner,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Manager,"),
                               str_remove(title, ".*Manager,"),
                               org_name),
             org_name = ifelse(str_dct(title, "City of"),
                               str_remove(title, ".*City of"),
                               org_name),
             org_name = ifelse(str_dct(title, "Treasurer,"),
                               str_remove(title, ".*Treasurer,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Consultant,"),
                               str_remove(title, ".*Consultant,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Attorney General,"),
                               str_remove(title, ".*Attorney General,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Engineer,"),
                               str_remove(title, ".*Engineer,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Judge,"),
                               str_remove(title, ".*Judge,"),
                               org_name),
             org_name = ifelse(str_dct(title, " CEO,"),
                               str_remove(title, ".*CEO,"),
                               org_name),
             org_name = ifelse(str_dct(title, ", Secretary,"),
                               str_remove(title, ".*, Secretary,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Attorney,"),
                               str_remove(title, ".*Attorney,"),
                               org_name),
             org_name = ifelse(str_dct(title, " \\(C.O\\),"),
                               str_remove(title, ".* \\(C.O\\),"),
                               org_name),
             org_name = ifelse(str_dct(title, " Chief Technology Officer,"),
                               str_remove(title, ".*Chief Technology Officer,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Governmental Affairs,"),
                               str_remove(title, ".*Governmental Affairs,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Mayor, "),
                               str_remove(title, ".*Mayor, "),
                               org_name),
             org_name = ifelse(str_dct(title, "Affairs,"),
                               str_remove(title, ".*Affairs,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Governor,"),
                               str_remove(title, ".*Governor,"),
                               org_name),
             org_name = ifelse(str_dct(title, " Operations,"),
                               str_remove(title, ".* Operations,"),
                               org_name),
             org_name = ifelse(str_dct(title, " Fellow,"),
                               str_remove(title, ".* Fellow,"),
                               org_name),
             org_name = ifelse(str_dct(title, "ember,"),
                               str_remove(title, ".*ember,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Board Member,"),
                               str_remove(title, ".*Board Member,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Chairman et al.,"),
                               str_remove(title, ".*Chairman et al.,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Officer,"),
                               str_remove(title, ".*Officer,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Secretary,"),
                               str_remove(title, ".*Secretary,"),
                               org_name),
             org_name = ifelse(str_dct(title, "Chair of the Board,"),
                               str_remove(title, ".*Chair of the Board,"),
                               org_name)
             )

d$title %<>% str_remove("Comment submitted by |Comment from |Comments from |Comment on |Comments|Submitted Electronically via eRulemaking Portal")

d$org_name%<>% str_squish()

save(d, file = here::here("data", "comments4datasheets.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))

# filter down to org comments
d %<>%
  group_by(docket_id, org_name) %>%
  add_count(name = "org_total") %>%
  ungroup() %>%
  arrange(-number_of_comments_received) %>%
  filter(attachment_count > 0,
         str_detect(str_c(title, org_name), "Congress|Senat|Rep\\.|Sen\\.|Representative") |
         # individual names
         !str_detect(org_name, "^.\\.$|^.\\. |^.\\.[A-Z][a-z]|^.\\. [A-Z][a-z]|^\\w+ .\\.|illegible|no surname"),
         !str_detect(title, "illegible|surname| suranme |last name|forename|no name"),
         nchar(org_name) > 1) %>%
  mutate(org_name = org_name %>% replace_na("NA") ) %>%
  filter(number_of_comments_received > 9 | !org_name %in% c("NA", "na", "","unknown")) %>%
  add_count(docket_id, name = "org_comments")


d %>% count(org_name, sort = T)

# random sample
d %>% distinct(org_name,title) %>% slice_sample(n = 100) %>% knitr::kable()

d %>%
  #filter(n > 10, n < 20) %>%
  count(docket_id, sort = T) %>% knitr::kable()

d %>% distinct(docket_id, org_comments)%>% knitr::kable()

d %>%  filter(org_comments < 1000)

## AUGMENT FUNCTION
# ad document name and link
d %<>%
  mutate(file_1 = ifelse(attachment_count > 0,
                         str_c(document_id, "-1.pdf"),
                         NA),
         attachment_txt = ifelse(attachment_count > 0,
                      str_c("https://ssc.wisc.edu/~judgelord/comment_text/",
                            document_id %>% str_remove("-.*$"), # agency folder
                            "/",
                            document_id %>% str_remove("-[A-z1-9]*$"), # docket folder
                            "/",
                            document_id,
                            "-1.txt"),
                      NA),
         comment_url = str_c("https://www.regulations.gov/comment/",
                             document_id),
         docket_url = str_c("https://www.regulations.gov/docket/",
                            document_id %>% str_remove("-[0-9]*$")))

d$attachment_txt[1]
d$comment_url[1]
d$docket_url[1]

d %<>% rename(comment_title = title)
names(d)
## PREP SHEETS
d %<>% select(#agency_acronym,
              docket_id,
              docket_url,
              #docket_title,
              document_id,
              posted_date,
              comment_url,
              comment_text,
              #attachment_txt,
              organization,
              comment_title,
              attachment_count,
              number_of_comments_received,
              org_name)

# add blanks
d %<>% mutate(position = "",
              position_certainty = "",
              comment_type = "",
              coalition_comment = "",
              coalition_type = "",
              # org_name = organization, # run scratchpad/orgnames.R until this is a function
              org_name_short = "",
              org_type = "",
              ask = "",
              ask1 = "",
              ask2 = "",
              ask3 = "",
              success = "",
              success_certainty = "",
              sucess1 = "",
              success2 = "",
              success3 = "",
              response = "",
              pressure_phrases = "",
              accept_phrases = "",
              compromise_phrases = "",
              reject_phrases = "",
              notes = "")

names(d)

# unique(d$organization)

count(d, organization, sort = T) %>% head()
count(d, org_name, sort = T) %>% head()

d %>% filter( organization  == "N/A") %>% distinct(org_name, number_of_comments_received)
d %>% filter(org_name == "unknown") %>% distinct(org_name, number_of_comments_received)

# create new directory if needed
if (!dir.exists(here::here("data", "datasheets") ) ){
  dir.create( here::here("data", "datasheets") )
}


write_comment_sheets <- function(docket){
  d %>%
    filter(docket_id == docket) %>%
    write_csv(file = here::here("data",
                                "datasheets",
                              #str_extract("^[A-Z]"), # agency
                              str_c(docket, "_org_comments.csv")))
}


names(d)
d %<>% mutate(comment_type = ifelse(number_of_comments_received > 99, "mass", comment_type))

unique(d$docket_id)

walk(unique(d$docket_id), write_comment_sheets)





