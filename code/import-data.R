source(here::here("setup.R"))

library(googledrive)
library(googlesheets4)

# all comment data from ______
if(!exists("comments_min")){
load(here("data", "comments_min.Rdata"))

  comments_min %<>% mutate(id = document_id)

comments_min %<>%
  group_by(id) %>%
  slice_max(order_by = number_of_comments_received, n = 1) %>%
  ungroup()
}

gs4_auth(email = "devin.jl@gmail.com")

drive_auth(email = "devin.jl@gmail.com")

# get all org comments sheets
s <- drive_find(pattern = "org_comment", type = "spreadsheet")

s$name
#FIXME
s$name
dockets_coded <- s$name %>% str_remove("_.*") |> sort()
dockets_coded

dockets_coded[which(dockets_coded %in% dockets)]


# drop Clean Power Plan rules that are not currently coded and will be problems:
s %<>% filter(!str_dct(name, "EPA-HQ-OAR-2013"))



# read all as char
read_sheet_c <- . %>% read_sheet(col_types = "c")

# init
d1 <- read_sheet_c(s[1,])

# map
d <- map_dfr(s$id, possibly(read_sheet_c, otherwise = head(d1)))

# FAILED TO IMPORT? OR BAD ID
fails <- s %>% filter(!name %>% str_remove("_.*") %in% d$docket_id)
fails


# in case some fail, try again
if(length(fails) > 0){
d2 <- map_dfr(fails$id, possibly(read_sheet_c, otherwise = head(d1)))


d %<>% full_join(d2) %>% distinct()
}


# STILL FAILED TO IMPORT? OR BAD ID
s$name[!s$name %>% str_remove("_.*") %in% d$docket_id]

# remove extra white space
d %<>% mutate_all(str_squish)

# which dockets are in the data
unique(d$docket_id)

sum(is.na(d$coalition_type))

# cache raw data
dtemp <- d

d <- dtemp



d %<>% distinct()

# duplicates
d %>%
  count(document_id, docket_id, sort = T) %>%
  filter(n>1) %>%
  group_by(docket_id) %>%
  summarise(n = sum(n)) %>%
  arrange(-n)

d %<>% mutate_all(str_squish)


# Import mass comments
source("code/import-mass.R")
d %<>% full_join(mass %>% filter(docket_type == "Rulemaking"))


# Import Congress
# Congress #TODO import congress 1HBjG32qWVdf9YxfGPEJhNmSw65Z9XzPhHdDbLnc3mYc
source("code/import-congress.R")
#FIXME join?

d %<>% select(-docket_type)

d %<>% distinct()

# use a variable in the mass comments sheet as an indicator of whether the observation came from an org_comments sheet or from the mass comments sheet
d %<>% mutate(source = ifelse(is.na(transparent), "datasheet", "mass"))

# duplicates
d %>%
  filter(comment_type != "elected") %>%
  count(document_id, docket_id, sort = T) %>%
  filter(n>1) %>%
  group_by(docket_id) %>%
  summarise(n = sum(n)) %>%
  arrange(-n)


# not the same in datasheet and mass sheet
duplicates <- d %>%
  add_count(document_id, sort = T) %>%
  filter(n>1,
         comment_type != "elected" # excluding elected officials (e.g., members of congress)
         ) %>%
  distinct(document_id, source) %>%
  add_count(document_id) %>%
  filter(n>1) %>%
  distinct(document_id) %>%
  left_join(mass %>% mutate(source = "mass"))%>%
  left_join(d %>% filter(source == "datasheet")) %>%
  select(source, document_id, names(mass))

duplicates %>%
  drop_na(position) %>%
  add_count(document_id) %>%
  filter(n>1) %>%
  kablebox()


duplicates %>%

duplicates %>%
  kablebox()

sum(is.na(d$document_id))
sum(is.na(d$comment_url))

d %>% filter(is.na(docket_id))

d %>% filter(!source == "mass", !docket_id %in% str_remove(s$name,"_.*")) %>% distinct(docket_id)

str_remove(s$name,"_.*")


# some dups with varitions in docket title
# inspect
d %>% filter(str_dct(coalition_comment, "greyhound")) %>% pull(success)

# some dups with varitions in docket title
d %<>% ungroup() %>% dplyr::select(-docket_title) %>% distinct()

# class
d %<>%
  na_if("NA") %>%
  na_if("na") %>%
  na_if("NaN") %>%
  # fix ids
  mutate(document_id = document_id %>% str_remove("-1.pdf") %>% str_squish(),
         comment_url = str_c("https://www.regulations.gov/comment/", document_id)) %>%
  # fix missing docket id
  mutate(docket_id2 = document_id %>% str_remove("-[0-9]+$")) %>%
  mutate(docket_id = coalesce(docket_id, docket_id2),
         docket_url = str_c("https://www.regulations.gov/docket/", docket_id),
         agency_acronym2 = str_remove(docket_id, "-[0-9].*"),
         agency_acronym = coalesce(agency_acronym, agency_acronym2),
         agency = agency_acronym) %>%
  # remove temp docket id from documents
  select(-docket_id2)# %>%  mutate(across(starts_with("success")), as.numeric )


# FAILED TO IMPORT
s$name[!s$name %>% str_remove("_.*") %in% d$docket_id]

sum(is.na(d$document_id))
sum(is.na(d$comment_url))



# Inspect
d %>% distinct(docket_id, docket_url)  %>% kablebox()

# comments where there is no docket id
d %>% filter(is.na(docket_id))

# to numeric
d %<>% mutate(success = success %>% str_squish() %>%  as.numeric())
d$success %<>% as.numeric()
d$position %<>% str_squish() %>% as.numeric()
d$number_of_comments_received %<>% str_squish() %>% as.numeric()

sum(is.na(d$number_of_comments_received))

# add missing comments
d %<>%
  left_join(comments_min %>%
              dplyr::select(document_id = id, ncomments = number_of_comments_received) %>%
              distinct())

d %<>% mutate(number_of_comments_received = coalesce(number_of_comments_received, ncomments)) %>%

  dplyr::select(-ncomments)

# check, should be 0 now
sum(is.na(d$number_of_comments_received))

d %>% filter(is.na(number_of_comments_received)) %>%
  kablebox()

# add dates
d %<>%
  left_join(comments_min %>%
              dplyr::select(document_id = id, date) %>%
              distinct())

sum(is.na(d$date))

d %<>% mutate(president = ifelse(date > as.Date("2009-01-20"), "Obama", "Bush"),
                           president = ifelse(date > as.Date("2017-01-20"), "Trump", president))

# POST - HOC DATE CORRECTIONS #TODO
# potentially problem dockets, likely bad dates = bad president
# some of these actually cross a president
d %>%
  count(docket_id, president)  %>%
  add_count(docket_id) %>%
  filter(nn>1)

d %<>% mutate(president = ifelse(docket_id %in% c("FEMA-2016-0003","OSHA-H005C-2006-0870"),
                     "Obama",
                     president))


# fix comment type and  org type
clean_org_type <- . %>%
  str_replace_all(":", ";") %>%
  str_replace_all("; ", ";") %>%
  str_to_lower() %>%
  str_squish()

d %>% filter(str_detect(coalition_comment, ";"))

# groups in more than one coalition
d %<>%
  mutate(coalition_comment = coalition_comment %>%
           # drop double-coded coalitions
           str_rm_all("Commercial Fishing;|Ocean Industries;|Sustainable Fisheries;|Fishing Industry;|Native Americans;|Regional Councils;|Government;|Auditors;|farmers;|hunters;|environmentalists;|environmental;|environment;|energy industry;") %>%
           # split group in multiple coalitoins
           str_split(";") ) %>%
  unnest(coalition_comment)


sum(is.na(d$coalition_type))

# Misssing position coding
d %>% filter(is.na(position), !is.na(coalition_comment)) %>%
  dplyr::select(document_id, coalition_comment, position) %>%
  kablebox()

d %<>% ungroup()

d %>% count(document_id, sort = T) %>% filter(n>1)

d %<>% add_count(document_id, name = "n_obs") %>%
  filter(!(source == "mass" & n_obs > 1))

duplicates <- d %>% filter(n_obs>1) %>%
  ungroup() %>%

  dplyr::select(document_id, starts_with("org"), starts_with("coalition_"), source) %>%
  arrange(document_id)



#FIXME - track down source of duplicates
d %>% ungroup() %>%
  mutate(source = replace_na(source, "datasheet")) %>%
  group_by(document_id) %>%
  slice(n = 1) %>%
  ungroup()

# Missingness
missingness <- d %>%
  filter(!str_dct(comment_type, "individual"),
         is.na(position) |is.na(coalition_comment) | is.na(org_type))%>%
  dplyr::select(document_id, comment_type, position, coalition_comment, org_name, org_type, success, source) %>%
  distinct()

# High priority to fix
missingness %>%
  filter(!is.na(success)) %>%
  kablebox()

d %>%
  filter(is.na(position) &is.na(coalition_comment) & is.na(success) & is.na(org_type)) %>%
  dplyr::select(position, coalition_comment, success, org_type) %>%
  distinct() %>%
  kablebox()

# docket - level summary vars
d %<>%
  filter(!is.na(position) |!is.na(coalition_comment) | !is.na(success) | !is.na(org_type)) %>%
  group_by(docket_id) %>%
  # drop extra info added after coalition, org type etc
  mutate(coalition_comment = coalition_comment %>% clean_org_type() %>% str_replace("false", "FALSE"),
         org_type = org_type %>% clean_org_type(),
         org_name = org_name %>% clean_org_type(),
         coalition_type = coalition_type %>% clean_org_type() %>% str_remove(";.*") %>% str_extract("^public|^private")) %>%
  # docket level summary
  mutate(coalitions = unique(coalition_comment) %>% length(),
         coalition_unopposed = abs(max(position, na.rm = T)-min(position,na.rm = T)) < 2,
         comments = number_of_comments_received %>% replace_na(1), #FIXME should be fixed
         congress = str_dct(org_type, "congress|house|senate") & str_dct(comment_type, "elected")) %>%
  mutate(org_type = ifelse(congress, str_replace_all(org_type, ";", ";;;;"), org_type)) %>%
  mutate(org_type = str_split(org_type, ";;;;")) %>%
  unnest(org_type) %>%
  group_by(docket_id, coalition_comment) #TODO all coalition vars here

sum(is.na(d$coalition_type))

# MOCs
comments_coded$congress %>% sum()

#FIXME just to make sure we are not dropping obs due to this reasonable assumption
d$congress %<>% replace_na(FALSE)

# diognostics
#NOTE: THIS IS BEFORE FILLING IN MISSING POSITIONS
missing_position <- d %>%
  filter(is.na(position), !is.na(org_type))

missing_position %>%
  filter(source =="datasheet") %>%
  ungroup() %>%
  dplyr::select(document_id, comment_type, starts_with("org"), position) %>% kablebox()



# uncoded mass
d %>% filter(comment_type == "mass",

             #source == "datasheet",
             is.na(coalition_comment)) %>%
  count(docket_id, source)



# COALITIONS CODED BOTH WAYS
d %>% ungroup() %>%

# coalitions with no main orgs coded
d %>%
  filter(source == "datasheet") %>%
  mutate(success = mean(success, na.rm = T))  %>%
  distinct(coalition_comment, success) %>%
  dplyr::select(docket_id, everything()) %>%
  filter(is.na(success), coalition_comment != "FALSE", !is.na(coalition_comment)) %>%
  kablebox()

d %>% filter(str_dct( coalition_comment, "gulf")) %>% dplyr::select(success)

# coalitions with positive and negative success
d %>%
  mutate(success_mean = mean(success, na.rm = T) )  %>%
  distinct(coalition_comment, success, success_mean) %>%
  dplyr::select(docket_id, everything()) %>%
  filter(!is.na(success), !success_mean >= 1, !success_mean <= -1 , coalition_comment != "FALSE", !is.na(coalition_comment)) %>%
  arrange(coalition_comment) %>%
  kablebox()

# coalition leaders with positive and negative success
d %>%
  filter(org_name == coalition_comment) %>%
  mutate(success_mean = mean(success, na.rm = T) )  %>%
  distinct(org_name, coalition_comment, success, success_mean, president) %>%
  dplyr::select(docket_id, everything()) %>%
  filter(!is.na(success), !success_mean >= 1, !success_mean <= -1 , coalition_comment != "FALSE", !is.na(coalition_comment)) %>%
  arrange(coalition_comment) %>%
  kablebox()

# Coalitions without leader
d %>%
  mutate(leader = coalition_comment %in% c(org_name, str_to_lower(org_name_short))) %>%
  distinct(leader) %>%
  add_count() %>%
  filter(n<2, coalition_comment != FALSE, !leader) %>%
  filter(org_name == coalition_comment) %>%
  mutate(success_mean = mean(success, na.rm = T) )  %>%
  distinct(org_name, coalition_comment, success, success_mean, president) %>%
  dplyr::select(docket_id, everything()) %>%
  filter(!is.na(success), !success_mean >= 1, !success_mean <= -1 , coalition_comment != "FALSE", !is.na(coalition_comment)) %>%
  arrange(coalition_comment) %>%
  kablebox()



d %>% filter(str_dct(coalition_comment, "greyhound")) %>% distinct(success)

d %>% filter(str_dct(coalition_comment, "greyhound")) %>% dplyr::select(success) %>% mutate(success = mean(success, na.rm = T))

# missing or extra coalition type

  filter(coalition_comment != "FALSE", is.na(coalition_type)) %>%
  dplyr::select(document_id, coalition_comment, coalition_type) %>%
  kablebox()

# where coalition = FALSE, but coalition_type is not blank
d %>%
  filter(coalition_comment == "FALSE", !is.na(coalition_type)) %>%
  dplyr::select(document_id, org_name, coalition_comment, coalition_type) %>%
  kablebox()


# miscoded elected as gov
d %>%
  filter(comment_type == "elected", org_type == "gov") %>%
  count(docket_id, comment_type, org_type) %>%
  kablebox()

d$comments %>% range(na.rm = T)

d %>% filter(comments == 999999) %>% pull(comment_url, document_id)

d %<>% filter(comment_url != "https://www.regulations.gov/comment/FMCSA-1997-2350-23794")

#FIXME THESE ARE FIXED IN THE MASS SHEET AND OTHERS, SO THOSE NUMBERS SHOULD BE USED FIRST
d %<>% mutate(comments = ifelse(document_id == "FWS-HQ-IA-2013-0091-1605",
                    1013942,
                    comments))



d$position %>% unique()


d$coalition_unopposed %>% unique()
d$congress %>% unique()



# this could just be d till the end
comments_coded <- d %>% mutate(comment_type = comment_type %>% clean_org_type() %>%
                             str_replace("gov", "org") %>%
                               str_replace("astroturf", "mass") %>%
                               str_replace("mass;corp campaign", "corp;corp campaign") %>%
                               str_replace("^indi.*al$", "individual") %>%
                             str_remove(";.*|-.*| o.*| .*"),
                           org_type = org_type %>% clean_org_type() %>%
                             str_replace("business", "corp") %>%
                             str_replace("nonprofit", "ngo") %>%
                             str_replace("ngo;ngo", "ngo") %>%
                             str_replace("corp;group", "corp group") %>%
                             str_replace("corps", "corp") %>%
                             str_replace("corp:corp", "corp") %>%
                             str_replace("^union", "ngo") %>%
                             str_replace("^tribe", "gov;tribe")) %>%
  ungroup()

# fill missing org then missing coalition success
# should be done after caluclating mean
comments_coded %<>%
  group_by(president, docket_id, coalition_comment, org_name) %>%
  fill(position, .direction = c("updown")  ) %>%
  fill(coalition_type, .direction = c("updown")) %>%
  ungroup() %>%
  group_by(president, docket_id, coalition_comment) %>%
  fill(org_type, .direction = c("updown")) %>%
  ungroup()



# still no position, that is
# no one in the coalition has a position
comments_coded %>% filter(is.na(position)) %>%
  distinct(document_id, coalition_comment, position) %>%
  kablebox()

# breakdown of types
comments_coded %>% count(comment_type, sort =T) %>%
  kablebox()

# breakdown of types
comments_coded %>% filter(is.na(comment_type) & comment_type %in% c("Org", "Elected")) %>%
  count(docket_id, sort =  T)

comments_coded %>% count(docket_id, comment_type, sort =T) %>%
  drop_na(comment_type) %>%
  filter(!comment_type %in% c("ngo", "astroturf", "corp", "corp group", "gov", "elected", "mass", "individual", "house", "senate", "org", "congress")) %>%
  kablebox()

# org type

comments_coded %>% count(org_type, sort =T) %>%
  kablebox()

# missing org type
filter(comments_coded,
       comment_type == "org",
       is.na(org_type) , source =="datasheet" # not from mass sheet
       ) %>%
  dplyr::select(docket_id, comment_type,
         starts_with("org"), coalition_comment) %>%
  kablebox()

# incorrect or or comment types
comments_coded %>%
  count(docket_id, org_type, comment_type, source, sort =T) %>%
  drop_na(org_type) %>%
  filter(comment_type != "elected",
         !str_dct(org_type, str_c("ngo", "astroturf", "corp", "corp group", "gov",
                                     "elected", "mass", "individual",
                                     "house", "senate", "state", "city", "assembly", "org", "congress", sep = "|"))) %>%

  kablebox()

# missing success
# I know some of these are in progress, but FYI these sheets are missing “success” for some org comments
comments_coded %>%
  filter(comment_type == "org"|comment_type == "Org") %>%
  group_by(docket_id, coalition_comment) %>%
  summarise(percent_success_coded = sum(!is.na(success))/n()) %>%
  ungroup() %>%
  arrange(percent_success_coded, docket_id) %>% kablebox()

comments_coded %>%
  filter(!is.na(ask)) %>%
  group_by(docket_id, coalition_comment) %>%
  summarise(percent_success_coded = sum(!is.na(success))/n()) %>%

  ungroup() %>%
  arrange(percent_success_coded) %>% kablebox()

# missing number of comments
comments_coded %>%
  filter(is.na(number_of_comments_received)) %>%
  count(docket_id) %>% kablebox()

sum(is.na(comments_coded$coalition_type))

# assume private if it is lobbying alone as a business
comments_coded %<>% mutate(business = str_dct(org_type, "^corp"),
                           ngo = str_dct(org_type, "^ngo"),
                           gov = str_dct(org_type, "^gov"),
                           coalition_type = ifelse(is.na(coalition_type) & coalition_comment == "FALSE" & business, "private", coalition_type),
                           coalition_type = ifelse(is.na(coalition_type) & coalition_comment == "FALSE" & (gov|ngo), "public", coalition_type) )

comments_coded %>% count(coalition_type)



#  elected
comments_coded %>%
  filter(comment_type == "elected") %>%
  count(org_type %>% str_remove("-.*|;.*"), sort = T) %>% kablebox()

# bad elected
comments_coded %>%
  mutate(org_type = org_type %>% str_remove("-.*|;.*")) %>%
  filter(comment_type == "elected",
         org_type %in% c("congress", "elected", "gov", "house of representatives",
                         "representative robert c. scott house",
                         "senator patty murray senate",
                         "senators lamar alexander senate",
                         "") |
           str_dct(org_type, "^rep|^senator")
         ) %>%
  dplyr::select(document_id, org_type) %>%
  distinct() %>%
  kablebox()



# Missing mass
comments_coded %>%
  filter(str_dct(comment_type, "mass") | number_of_comments_received > 100,
         is.na(coalition_comment),
         source =="datasheet") %>%
  distinct(comment_type, coalition_comment, document_id, number_of_comments_received) %>%
  kablebox()


# replace missing/non coalitions with org name
comments_coded %<>% mutate(coalition_comment = ifelse(coalition_comment=="FALSE", org_name, coalition_comment))


# COALITION-LEVEL VARS
library(DescTools)
comments_coded %<>%
  ungroup() %>%
  group_by(president, docket_id, coalition_comment) %>% # FIXME where dates are wrong, president is wrong
  mutate(coalition_congress = sum(congress),
            coalition_size = unique(org_name) %>% length(),
            coalition_position = mean(position, na.rm =T),
            #FIXME coalition_private and coalition_buisness are two ways of running this
            coalition_business = sum(business),
            coalition_type =  Mode(coalition_type, na.rm = T)[1] %>% as.character(),
            coalition_success = mean(success %>% as.numeric(), na.rm = T), # average success of coalition
            org_lead = coalition_comment %in% c(org_name, str_to_lower(org_name_short)),
            coalition_leader_success = ifelse(org_lead, success, NA) %>% mean(na.rm = T) %>% coalesce(coalition_success),
            coalition_comments = sum(comments, na.rm = T) - coalition_size, # subtracts coalition size
         coalition_comment_types = unique(comment_type) %>%
           purrr::discard(is.na) %>% #FIXME #FIXED?
           str_c(collapse = ";"),
         coalition_campaign_ = str_dct(coalition_comment_types, "mass") | coalition_comments > 99,
         coalition_ = coalition_size > 1,
         coalition_business_ = coalition_business/coalition_size > .5) %>%
  ungroup() %>%
  mutate(coalition_id = paste(president, docket_id, coalition_comment) %>%
           as.factor() %>%
           as.numeric()) %>%
  distinct()

if(F){
# lower bound proxi for mass where number is not reported
comments_coded %<>% mutate(coalition_comments =  ifelse(str_dct(coalition_comment_types, "mass") & coalition_comments < 100, coalition_comments + 100, coalition_comments))

# don't count duplicate uploads as mass
comments_coded %<>% mutate(coalition_comments =  ifelse(!str_dct(coalition_comment_types, "mass") & coalition_comments < 100, 0, coalition_comments))
}

# inspect

# large coalitions
comments_coded %>%
  arrange(-coalition_comments) %>%
  distinct(docket_id, coalition_comment, coalition_type, coalition_comments) %>%
  kablebox()


comments_coded %>%
  filter(source =="datasheet") %>%
  distinct(docket_id,
           coalition_comment_types,
           coalition_comment,
           coalition_campaign_, coalition_comments,
           coalition_success, source) %>%
  arrange(-coalition_comments) %>%
  kablebox()

comments_coded %>% filter(!coalition_campaign_, str_dct(comment_type, "mass")) %>%
  distinct(comment_type, coalition_comment_types, coalition_comment, coalition_campaign_) %>% kablebox()

distinct(comments_coded, coalition_id, coalition_type, coalition_comment)

distinct(comments_coded, docket_id, coalition_type, coalition_comment) %>%
  filter(is.na(coalition_type), !is.na(coalition_comment)) %>% kablebox()


comments_coded %>% filter(is.na(coalition_type))

sum(is.na(comments_coded$coalition_type))

comments_coded$coalition_type %>% unique()

median(comments_coded$coalition_type, na.rm = T)

# Should be the same, but I don't understand why mean worked here and not above
comments_coded %>%
  count(coalition_type)

comments_coded %>%
  group_by(president, docket_id, coalition_comment) %>%
  mutate(x =  mean(coalition_type, na.rm = T)[1] %>% as.character()) %>%
  ungroup() %>%
  count(coalition_type)

# FOR PRESENTATION
comments_coded %<>% mutate(Coalition_size = case_when(
  coalition_size==1 ~ "1",
  coalition_size > 1 & coalition_size < 11 ~ "2-10",
  coalition_size > 10 & coalition_size < 101~ "11-100",
  coalition_size > 100 ~ "More than 100"
))

comments_coded %<>% mutate(Coalition_comments = case_when(
  coalition_comments == 0 ~ "0",
  coalition_comments > 0 & coalition_comments < 101 ~ "1-99",
  coalition_comments > 100 ~ "More than 99"
))

comments_coded %<>% mutate(Coalition_campaign = ifelse(coalition_campaign_,
                                                       "Mass comments",
                                                       "No mass comments"))

#TODO import congress 1HBjG32qWVdf9YxfGPEJhNmSw65Z9XzPhHdDbLnc3mYc

comments_coded %<>% mutate(Coalition_business = ifelse(coalition_business_, "Business", "Non-business"))

comments_coded %<>% mutate(Position = case_when(
  position < 3  ~ "Opposes rule",
  position > 2 ~ "Supports rule",
  position == 6 ~ "Opooses rule as insufficient"
))

comments_coded %>% dplyr::select(position, Position)

comments_coded %<>% mutate(Coalition_Position = case_when(
  coalition_position < 3  ~ "Opposes rule",
  coalition_position > 2 ~ "Supports rule",
  coalition_position == 6 ~ "Opooses rule as insufficient"
))

comments_coded %>% dplyr::select(coalition_position, Coalition_Position)

comments_coded %>% count(coalition_type)

# should just be elected comments that were split
comments_coded %>% add_count(document_id, sort = T) %>%
  filter(n>1) %>%
  count(docket_id)

comments_coded %>% add_count(document_id, sort = T) %>%
  filter(n>1) %>%
  kablebox()

comments_coded %>% dplyr::select(-comment_text)

comments_coded %>% group_by(document_id) %>% slice_head()

# relevel president for better analysis
comments_coded %<>%
  mutate(party = ifelse(president == "Obama", "Democrat", "Republican"),
         president = as_factor(president) %>% relevel(ref = "Obama"))




# SUCCESS MUST BE DONE AFTER MAKING COALTION VARS
comments_coded %<>%
  group_by(docket_id, president, coalition_comment) %>%
  fill(success, .direction = "updown") %>%

  fill(position, .direction = "updown")


# check after fill
comments_coded %>%
  filter(is.na(coalition_type), !is.na(success)) %>%
  #filter(agency_acronym != "ED") %>%
  distinct(docket_id, coalition_comment, coalition_type, org_type, org_name) %>%
  kablebox()

sum(is.na(comments_coded$coalition_type))


# Misssing position coding after fill
comments_coded %>% filter(is.na(position), !is.na(coalition_comment)) %>%
  dplyr::select(document_id, coalition_comment, position) %>%
  kablebox()


# uncoded mass after fill
comments_coded %>% filter(comment_type == "mass",
             #source == "datasheet",
             is.na(coalition_comment)) %>%
  count(docket_id)


# common names
comments_coded %<>%
  mutate(coalition = coalition_comment,
         agency = str_remove(docket_id, "-.*"))%>%
  mutate(org_name = org_name %>% str_to_title())%>%
  mutate(coalition = coalition %>% str_to_title()) %>%
  mutate(campaign_ = coalition_campaign_)


comments_coded$coalition_type %<>% str_to_title()
comments_coded$Coalition_Position %<>% str_to_title()
##########################################################################

# MAKE COALITIONS
# (anything after here should be coalition-specific)
coalitions_coded <- comments_coded %>%
  drop_na(coalition_comment) %>%
  dplyr::select(president, party, agency, starts_with("docket"), starts_with("coalition")) %>%
  distinct()

# just to make sure
comments_coded %<>% distinct() %>% ungroup()
coalitions_coded %<>% distinct() %>% ungroup()


# common names
coalitions_coded %<>%
  mutate(comments = coalition_comments,
         Comments = Coalition_comments,
         campaign_ = coalition_campaign_)



##########################################################
# inspect - FROM HERE DOWN, JUST INSPECTING
coalitions_coded %>% kablebox()

d %>% filter(success > 2) %>% dplyr::select(document_id, success)

ggplot(coalitions_coded, aes(x = coalition_success)) + geom_histogram()+
  labs(x = "Coalition Success")

ggplot(coalitions_coded, aes(x = coalition_business %>% as.numeric())) + geom_histogram()+ labs(x = "Business Coalition")

ggplot(coalitions_coded %>% filter(!is.na(coalition_type)),
       aes(x = coalition_type)) +
  geom_histogram(stat = "count")+

  labs(x = "Coalition Type",
       title = "Number of Observations\nby Coalition Type and Size\n(number of organizations)") +
  facet_wrap("Coalition_size")

ggplot(coalitions_coded %>% filter(!is.na(coalition_type)), aes(x = coalition_type)) +
  geom_histogram(stat = "count")+
  labs(x = "Coalition Type",
       title = "Number of Observations\nby Coalition Type and Size\n(number of mass comments)") +
  facet_wrap("Coalition_comments")

ggplot(coalitions_coded, aes(x = coalition_size)) + geom_histogram()+ labs(x = "Coalition size")

#ggplot(coalitions_coded, aes( x= comment_length)) + geom_histogram()+ labs(x = "% (Comment length/proposed rule length)*100")

ggplot(coalitions_coded, aes( x= log(coalition_comments + 1))) + geom_histogram() + labs(x = "Log(comments) + 1")

# these should be the same
d$number_of_comments_received %>% sum(na.rm = T)
comments_coded %>% distinct(document_id, number_of_comments_received) %>% tally(number_of_comments_received)
# should be close to the same, with d a bit bigger perhaps
coalitions_coded$coalition_comments %>% sum()
coalitions_coded %>% distinct(coalition_id, coalition_comments) %>% tally(coalition_comments)

coalitions_coded %>% add_count(coalition_id) %>%

  arrange(coalition_id) %>%
  filter(n > 1)# %>% kablebox()


# duplicates
duplicates  <- comments_coded %>% add_count(document_id, sort = T) %>%

  filter(n>1, comment_type != "elected")

duplicates %>% kablebox()

duplicates %>%
  count(docket_id)

# Obama dockets that Trump used
coalitions_coded %>% add_count(docket_id, coalition_comment, sort = T) %>%
  filter(n>1) %>% arrange(coalition_comment) %>% distinct(president, docket_id)

look <- coalitions_coded %>% add_count(docket_id, president, coalition_comment, sort = T) %>%

  filter(n>1) %>% arrange(coalition_comment) %>% distinct()

look %>% distinct(docket_id, coalition_comment, coalition_type)


# FAILED TO GET THROUGH
s$name[!s$name %>% str_remove("_.*") %in% comments_coded$docket_id]
s$name[!s$name %>% str_remove("_.*") %in% coalitions_coded$docket_id]

# CHECK THESE IN PROGRESS, AS OF 7/11, not coded
# "USCIS-2015-0008_org_comments" "DEA-2020-0023_org_comments"   "WHD-2015-0001_org_comments"


unique(comments_coded$president)
unique(coalitions_coded$president)




#/INSPECT

# TODO merge in mass comments that were not hand-coded

# just to make sure

comments_coded %<>% distinct() %>% ungroup()
coalitions_coded %<>% distinct() %>% ungroup()

save(comments_coded, file = here::here("data", "comments_coded.Rdata"))
save(coalitions_coded, file = here::here("data", "coalitions_coded.Rdata"))
save(mass_coded, file = here::here("data", "mass_coded.Rdata"))
save(mass_raw, file = here::here("data", "mass_raw.Rdata"))

congress_coded <- comments_coded %>% filter(congress)

save(congress_coded, file = here::here("data", "congress_coded.Rdata"))
write_csv(congress_coded, file = here::here("data", "congress_coded.csv"))


comments_coded$docket_id %>% unique()

comments_coded %>% distinct(campaign_, comments, coalition)

#FIXME missing number of comments, odd since number of comments recieved is not NA, not sure where NA is coming from
comments_coded %>%
  filter(is.na(comments)|comments == "NA") %>%
  filter(document_id %in% comments_min$id) %>% # check both ways
  dplyr::select(document_id, comment_url, org_name, coalition, comments, number_of_comments_received, campaign_, success)

filter(coalitions_coded, coalition_comments < 0)
filter(coalitions_coded, comments < 0)
filter(comments_coded, coalition_comments < 0)
coalitions_coded$comments

#1 PRIORITY TO CODE!!!
d %>% filter(number_of_comments_received>99 | comment_type == "mass",
             source =="datasheet",
             is.na(coalition_comment)) %>% dplyr::select(document_id, comments) %>%  kablebox()

# including mass
d %>% filter(number_of_comments_received>99,
             is.na(coalition_comment)) %>% count(docket_id, source)

# hand-coded dockets
dockets<- comments_coded %>% filter(source =="datasheet") %>% pull(docket_id) %>% unique()

#2 Priority, in mass sheet
mass_raw %>%
  filter(docket_id %in% dockets, is.na(coalition_comment)) %>%
  count(docket_id, comment_url) %>% kablebox()

mass_raw %>%
  filter(docket_id %in% dockets, is.na(coalition_comment)) %>%
  count(docket_id, sort = T) %>% kablebox()

mass_raw$number_of_comments_received %>% as.numeric() %>% sum(na.rm  = T)

# no mass comments
comments_coded %>%
  group_by(docket_id) %>%
  summarise(docket_comment_types = str_c(unique(comment_type), collapse = ";"),
         docket_comments = sum(comments)) %>%
  filter(!str_dct(docket_comment_types, "mass"))

comments_coded %>% filter(comment_type == "elected",
                          !str_dct(org_type, "senate|house|governor|official|attorney general|school|mayor|city|county|state|assembly|officer")) %>%
  dplyr::select(comment_type, org_type, document_id) %>%

  kablebox()
