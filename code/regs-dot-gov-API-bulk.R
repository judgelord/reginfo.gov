library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)

source(here::here("functions", "regulations-gov-API-search.R"))

source(here::here("api-key.R"))

# RULES 
rules <- search.docs(documenttype = "FR" , 
                 n = 100000)
nrow(rules)
names(rules)
head(rules$postedDate)
tail(rules$postedDate)

save(rules, file = here::here("data", str_c("rules", Sys.Date(), ".Rdata")))

# NPRMS 
nprms <- search.docs(documenttype = "PR" , 
                  n = 100000)

nrow(nprms)
names(nprms)
head(nprms$postedDate)
tail(nprms$postedDate)

save(nprms, file = here::here("data", str_c("nprms", Sys.Date(), ".Rdata")))

notices <- search.docs(documenttype = "N" , 
                     n = 100000)

nrow(notices)
names(notices)
head(notices$postedDate)
tail(notices$postedDate)

save(notices, file = here::here("data", str_c("notices", Sys.Date(), ".Rdata")))


# Other 
other <- search.docs(documenttype = "O" , 
                       n = 100000)

nrow(other)
head(other$postedDate)
tail(other$postedDate)

save(other, file = here::here("data", str_c("other", Sys.Date(), ".Rdata")))

# SR 
support <- search.docs(documenttype = "SR" , 
                     n = 100000)

nrow(support)
head(support$postedDate)
tail(support$postedDate)

save(support, file = here::here("data", str_c("support", Sys.Date(), ".Rdata")))

# EJ

# EJ NPRMs
# test with first 10k
ejPR <- map_dfr(.x = c(1:10),
                .f = search_keyword_page,
                documenttype = "PR",
                keyword = "environmental justice")

ejPR %>%
  filter(!is.na(postedDate)) %>%
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page)

# # up to 100k
ejPR100 <- map_dfr(.x = c(11:100),
                 .f = search_keyword_page,
                 documenttype = "PR",
                 keyword = "environmental justice")

# # inspect
ejPR100 %>%
  filter(!is.na(postedDate)) %>%
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page)
#
ejPR %<>% full_join(ejPR100)

save(ejPR, file = here::here("data", 
                             str_c("ejPR", 
                                   Sys.Date(), 
                                   ".Rdata")))

##################################
# EJ Rules 
# test with first 10k
ejFR <- map_dfr(.x = c(1:10),
                      .f = search_keyword_page4,
                      documenttype = "Rule",
                      keyword = "environmental justice")

# up to 100k
ejFR2 <- map_dfr(.x = c(11:100),
                 .f = search_keyword_page4,
                 documenttype = "Rule",
                 keyword = "environmental justice")

# inspect
ejFR2 %>% 
  filter(!is.na(postedDate)) %>% 
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page)

ejFR2 %>% count(documentType)

ejFR %>% 
  mutate(year = str_sub(postedDate, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = documentType) +
  geom_bar()

ejFR %<>% full_join(ejFR2)

ejFR %>% filter(is.na(postedDate)) %>% count(docketId, message, sort= T)

save(ejFR, file = here::here("data", 
                             str_c("ejFR", 
                                   Sys.Date(), 
                                   ".Rdata")))



###################
# EJ COMMENTS 

# test with first 10k
ejcomments <- map_dfr(.x = c(1:10),
                      .f = search_keyword_page4,
                      documenttype = "Public Submission",
                      keyword = "environmental justice")

range(ejcomments$postedDate)

# up to 100k
ej100 <- map_dfr(.x = c(11:100),
                 .f = search_keyword_page4,
                 documenttype = "Public Submission",
                 keyword = "environmental justice")

# inspect
ej100 %>% 
  filter(!is.na(postedDate)) %>% 
  arrange(postedDate) %>%
  head() %>%
  select(postedDate, page, documentId)

ej100 %>% 
  filter(!is.na(postedDate)) %>% 
  mutate(year = str_sub(postedDate, 1,4) %>% as.numeric()) %>% 
  ggplot() + 
  aes(x = year, fill = documentType) +
  geom_bar()

ejcomments %<>% full_join(ej100)

save(ejcomments, 
     file = here::here("data", 
                       str_c("ejcomments", 
                             Sys.Date(), 
                             ".Rdata")))

# # up to .5m if needed (but as of 2020, n = 41,591k)
# ej500 <- map_dfr(.x = c(101:500),
#                  .f = search_keyword_page,
#                  documenttype = "PS",
#                  keyword = "environmental justice")
# 
# ejcomments %<>% full_join(ej500)
# 
# save(ejcomments, file =  here::here("data", "ejcomments.Rdata"))
