source("setup.R")


agency <- "EPA"
docket <- "EPA-HQ-OAR-2021-0317"

# LOAD DATA FROM DATA FOLDER
# comment metadata
here::here("data",
           agency,
           docket,
           paste(docket, "comments.rda", sep = "_")) |>
  str_replace("rulemaking", "regulationsdotgov-data") |>
  load()

# comment metadata (details )
here::here("data", agency, docket,
paste(docket, "comment_details.rda", sep = "_")) |>
  str_replace("rulemaking", "regulationsdotgov-data") |>
  load()

comment_details %<>% left_join(comments |> rename(document_subtype = subtype))

d <- comment_details |> group_by(id) |>  mutate(attachment_count = unlist(attachments) |> length()/3)

d$attachment_count %>% head()

#FIXME with updated org_names from hand-coding

names(d)

# rename regulations.gov
namingthings <- function(x){
  names(x)  <- names(x) %>%
    str_replace_all("([A-Z])", "_\\1") %>%
    str_to_lower()

  return(x)
}
  # standardize
d %<>% namingthings()

d %<>% mutate(organization = title,
              comment_text = comment,
              agency_acronym = agency_id,
              document_id = id)

nonorgs <- "^personal$|illegible|default organization name|^anonymous|individual$|myself|^mr$|^ms$|^mrs$|^mr.$|^ms.$|^mrs.$|me, myself, and i|not applicable|a youtuber|retired federal employee|the human race|the american people$|^the people$|truck driver$|^human race|^attorney at law|^na$|n/a|no name|^unknown|^none$|^none |noneindividual|^my |^myself|^self$|seslf|private citizen|^citizen.$|^select$|^attorney$| owner$|christian$|catholic$|please select|the dodo|^other$|individual$|just me|anonymous|retired$|citizen$|citzen|^citizens$|citizen/consumer|^citizen, |^citizen of|^citizen at|^citizen -|^citizen and|me, myself and i|me myself and i|^no organization|concerned american|self.employed|self and|regulations.gov|public comment|private party|private owner|private citizan|^private$|personal (comment|opinion)| taxpayer$| voter$| human$|^concerned citizens$|^consumer$| consumer$| mom"


d %<>% filter(attachment_count > 0,
              !str_detect(organization, nonorgs),
              !str_detect(organization, "^.\\. |illegible|surname|last name|forename|no name|^unknown$"),
              !str_detect(title, "illegible|surname|last name|forename|no name") )
dim(d)



# apply auto-coding
#FIXME with updated org_names from hand-coding


source(here::here("code", "org_name.R"))

#FIXME source(here::here("code", "comment_position.R"))

save(d, file = here::here("data", "temp_comments4datasheet.Rdata"))
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

save(d, file = here::here("data", "temp_comments4datasheet.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))

d %<>% mutate(number_of_comments_received = duplicate_comments)

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
  mutate(
  # file_1 = ifelse(attachment_count > 0,
  #                        str_c(document_id, "-1.pdf"),
  #                        NA),
  #        attachment_txt = ifelse(attachment_count > 0,
  #                                str_c("https://ssc.wisc.edu/~judgelord/comment_text/",
  #                                      document_id %>% str_remove("-.*$"), # agency folder
  #                                      "/",
  #                                      document_id %>% str_remove("-[A-z1-9]*$"), # docket folder
  #                                      "/",
  #                                      document_id,
  #                                      "-1.txt"),
  #                                NA),
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
                                agency,
                                #str_extract("^[A-Z]"), # agency
                                str_c(docket, "_org_comments.csv")))
}


names(d)
d %<>% mutate(comment_type = ifelse(number_of_comments_received > 99, "mass", comment_type))

unique(d$docket_id)

walk(unique(d$docket_id), write_comment_sheets)

