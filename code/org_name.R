# this script extracts some basic org_name info, prior to hand coding
#
# # FIXME
# source("setup.R")
#
#
# d <- filter(comments_all) %>%
#   #FIXME
#   filter(!is.na(organization), organization != "")
# filter(is.na(organization & is.na(title))) %>%
#   filter(organization != "" & title != "")
# # FIXME

# start with existing self-identified organization
d %<>% mutate(org_name = organization)

# remove preface in title
preface <- ".*ponsored by |.*ponsoring organization |.*ubmitted by |.*omments from |.*ampaign from |.*on behalf of "

d %<>%
  mutate(org_name =
           ifelse(grepl(preface,title) &
                    (is.na(org_name) | str_detect(org_name, "nknown") ),
                  gsub(preface,"",title), org_name) ) %>%
  mutate(org_name = ifelse( str_detect(org_name, "nknown|^NA$|^n/a$"),NA, org_name))


# clean up
d %<>% mutate(org_name = str_rm(org_name, "^comment from|^comment submitted by|^from|^re |- comment$|-comment$|comment$|^request for extension|^request for an extension"))


# # Look for bad org names to remove
# d %>%
#   filter(str_dct(org_name, str_c("^none$", "^unknown$", "^individual$", "^citizen$", "self$", "not applicable", "^private$", "personal", "lover", "mr\\.$", "mrs\\.$", "^ms$",
#                                                    "retired", "^dr$", "^miss$", "^mr$", "^ms\\.$", "^mr\\.$", "^na$", "^me$", "^-$|^--$", "street$", "^happy$", "^r$", "^home$", "please select", "^brain$", "^no name$",
#                                                    "no one$", "^nol longer", "no organization$", "- select -", "- none - ","--none--", "concerned citizen$", "-none-", "select\\.\\.\\.", "send$", "^love$", "^n\\.a\\.$",
#                                                    "^\\.$", "not specified", "^other$", "foekf", "what a shame this is", "no affiliation", "^usa$", "^LFJJHK$", "none\\. plain ol' concerned citizen.", "anonymous anonymous",
#                                                    "the federal government institute", "individual taxpayer", "citizen of the united states|citizen of the US", "public land owner", "^taxpayer", "american citizen",
#                                                    "^U\\.S\\. citizen$", "^unknown", "^farmer$", "^f u$", "^no$", "\\?",
#                                                    sep = "|"))) %>%
#   count(organization, org_name) %>% knitr::kable()

# Remove bad org names
d %<>%
  #mutate(org_name= tolower(org_name)) %>%
  mutate(org_name = ifelse(str_dct(org_name, str_c("^none$", "^unknown$", "^individual$", "^citizen$", "self$", "not applicable", "^private$", "personal", "lover", "mr\\.$", "mrs\\.$", "^ms$",
                                                   "retired", "^dr$", "^miss$", "^mr$", "^ms\\.$", "^mr\\.$", "^na$", "^me$", "^-$|^--$", "street$", "^happy$", "^r$", "^home$", "please select", "^brain$", "^no name$",
                                                   "no one$", "^nol longer", "no organization$", "- select -", "- none - ","--none--", "concerned citizen$", "-none-", "select\\.\\.\\.", "send$", "^love$", "^n\\.a\\.$",
                                                   "^\\.$", "not specified", "^other$", "foekf", "what a shame this is", "no affiliation", "^usa$", "^LFJJHK$", "none\\. plain ol' concerned citizen.", "anonymous anonymous",
                                                   "the federal government institute", "individual taxpayer", "citizen of the united states|citizen of the US", "public land owner", "^taxpayer", "american citizen",
                                                   "^U\\.S\\. citizen$", "^unknown", "^farmer$", "^f u$", "^no$", "\\?",
                                                   sep = "|")), NA, org_name))





# clean orgs
# d %<>% mutate(org_name = ifelse(is.na(org_name), title, org_name) )
# d$org_name <- gsub(".*ponsored by |.*ponsoring org_name |.*ampaign from |.*ubmitted by |.*omment from |.* on behalf of ", "", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*MoveOn.*", "MoveOn", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*CREDO.*", "CREDO Action", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*NRDC.*", "Natural Resources Defense Council", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Care2.*", "Care2", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Environmental Working|EWG.*", "Environmental Working Group", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Earthjustice.*", "Earthjustice", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Sierra Club.*", "Sierra Club", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Environmental Action.*", "Environmental Action", d$org_name, ignore.case = TRUE)
d$org_name <- gsub("Opportunties", "Opportunities", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*EDF.*", "Environmental Defense Fund", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Environmental Defense.*", "Environmental Defense Fund", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Monsanto.*", "Monsanto", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*American Lung Association.*", "American Lung Association", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Green For All.*", "Green For All", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Biological Diversity.*", "Center for Biological Diversity", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Audubon.*", "National Audubon Society", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Environment Maryland and Environment Virginia.*", "Environment Maryland and Environment Virginia", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Public Interest Research Group.*|.*tudent PIRG.*|.*PIRG..tudent.*", "Public Interest Research Group", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Unitarian*|unitarian.*", "Unitarian Congregations", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*American Policy Center.*", "American Policy Center", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Asbestos Disease Awareness org_name.*", "Asbestos Disease Awareness org_name", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Consumers Union.*", "Consumers Union", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*National Wildlife Federation.*", "National Wildlife Federation", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*350.*", "350", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Conservation Voters.*", "League of Conservation Voters", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Association for Gun Rights.*", "National Association for Gun Rights", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Friends of the Earth.*", "Friends of the Earth", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Center for Effective Government.*", "Center for Effective Government", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Interfaith Power and Light.*", "Interfaith Power and Light", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*Planned Parenthood.*", "Planned Parenthood", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*MomsRising|mom's rising.*", "MomsRising", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(".*PEW.*", "PEW", d$org_name, ignore.case = FALSE)
d$org_name <- gsub(".*Farm Bureau.*", "Farm Bureau", d$org_name, ignore.case = FALSE)
d$org_name <- gsub(".*Organizing for Action.*", "Organizing For Action", d$org_name, ignore.case = FALSE)

# second to last
d$org_name <- gsub(".*members of ", "", d$org_name, ignore.case = FALSE)
d$org_name <- gsub(".*Change.org.*", "Change.org", d$org_name, ignore.case = FALSE)

d$org_name <- gsub(".*unknown.*|^N.A$", "unknown", d$org_name, ignore.case = TRUE)
# do this last
d$org_name %<>% str_squish()
d$org_name <- gsub("^the |^ | $", "", d$org_name, ignore.case = TRUE)
d$org_name <- gsub("\\(.*", "", d$org_name, ignore.case = TRUE)
d$org_name <- gsub("\\. Sample.*|\\. \n.*|\n.*|\\,.*", "", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(" et al.*| - .*", "", d$org_name, ignore.case = TRUE)
d$org_name <- gsub(" \\(.*| \\[.*", "", d$org_name, ignore.case = TRUE)
d$org_name %<>% str_squish()

################################################################################


#org_name variable
########################################################################################################
#load in orgs
source("data/onewordorgs.R")

d %<>%
  # generally people's initials
  mutate(org_name = ifelse(str_dct(org_name, "^[[:alpha:]]\\. \\w+") & !str_dct(org_name, orgsShort), NA, org_name)) %>%
  mutate(org_name = ifelse(str_dct(org_name, "^[[:alpha:]]\\.\\w+") & !str_dct(org_name, orgsShort), NA, org_name)) %>%
  #making lost short org_name names stay as orgs while getting rid of useless one words
  mutate(org_name = ifelse(str_dct(org_name, "^\\w+$") & !str_dct(org_name, orgsShort), NA, org_name))


#broader rules
#############
#d %<>% mutate(title = comment_title)

d %<>%
  #sponsored by
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "sponsor... by [[:upper:]]"),
                          str_rm_all(title, ".* sponsor... by|\\(.*"),
                          org_name)) %>%
  #association
  mutate(org_name = ifelse(is.na(org_name) & str_dct(title, "association|assn") & !str_dct(title, "association of"),
                           str_rpl(title, "association.*", "Association"),
                           org_name)) %>%
  #association of
  mutate(org_name = ifelse(is.na(org_name) & str_dct(title, ".*association of.*"),
                           str_rpl(title, ".*association", "Association"),
                           org_name)) %>%
  #company
  mutate(org_name = ifelse(is.na(org_name) & str_dct(title, "company"),
                           str_rpl(title, "company.*", "Company"),
                           org_name)) %>%
  #conservancy
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "conservancy"),
                          str_rpl(title, ".*company", "conservancy"),
                          org_name)) %>%
  #department
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "department"),
                          str_ext(title, ".*department.*"),
                          org_name)) %>%
  #center
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*Center"),
                          str_ext(title,"\\w+ center.*"),
                          org_name)) %>%
  # #community1
  # mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "community") & !str_dct(title, "community$"),
  #                         str_ext(title, "\\w+ community.*"),
  #                         org_name)) %>%
  #community2
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "community"),
                          title,
                          org_name)) %>%
  #cooperative
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*cooperative"),
                          str_rpl(title, "cooperative.*", "Cooperative"),
                          org_name)) %>%
  #commission
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*commision|commission"),
                          str_rpl(title, "commision.*|commission.*", "Commission"),
                          org_name)) %>%
  #co.
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*Co\\.$"),
                          str_rm(title, "Co\\."),
                          org_name)) %>%
  #corp
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "corp |corp\\."),
                          str_rm(title, "corp |corp\\."),
                          org_name)) %>%
  #Inc.
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*Inc\\..*|.*Inc \\.*"),
                          str_rm(title, "Inc\\..*"),
                          org_name)) %>%
  #LLP
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "LLP"),
                          str_rpl(title, "LLP.*", "LLP"),
                          org_name)) %>%
  #LLC
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "LLC|LC"),
                          str_rpl(title, "LLC.*", "LLC"),
                          org_name)) %>%
  #coalition
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "coalition"),
                          str_ext(title, "(\\w+ )*coalition"),
                          org_name)) %>%
  #institute
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "institute|insitute"),
                          str_ext(title, ".*institute"),
                          org_name)) %>%
  #academy
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "academy of"),
                          str_ext(title, "academy of.*"),
                          org_name)) %>%
  #society1
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "society$"),
                          str_ext(title,  "(\\w+ )*society"),
                          org_name)) %>%
  #society2
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "society"),
                          str_ext(title,  "\\w+ society \\w+ \\w+"),
                          org_name)) %>%
  #alliance
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "alliance"),
                          str_ext(title, ".*alliance"),
                          org_name)) %>%
  #growers
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "growers"),
                          str_ext(title, "(\\w+ )*growers"),
                          org_name)) %>%
  #council, could use fixing
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "council"),
                          str_ext(title, ".* council"),
                          org_name)) %>%
  #mass mailer campaign
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*mass mailer campaign"),
                          str_rm(title, "mass mailer campaign.*"),
                          org_name)) %>%
  #member mass email
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*member mass email"),
                          str_rm(title, "member mass email.*"),
                          org_name)) %>%
  #comment from
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "comment from") & agency_acronym == "EPA",
                          str_rm(title, ".*comment from"),
                          org_name)) %>%
  #schools
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "school"),
                          str_rpl(title, "school.*", "School"),
                          org_name)) %>%
  #farms
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*farm"),
                          str_ext(title, "(\\w+ )*farm"),
                          org_name)) %>%
  #farms2
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*farm"),
                          str_ext(title, "\\w+ \\w+\\. farm"),
                          org_name)) %>%
  #farms3
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*farm"),
                          str_ext(title, ".*farm"),
                          org_name)) %>%
  #federation1
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "federation$"),
                          str_ext(title, "(\\w+ )*federation"),
                          org_name)) %>%
  #federation2
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "federation"),
                          str_ext(title, "\\w+ federation.*"),
                          org_name)) %>%
  #church
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*church"),
                          title,
                          org_name)) %>%
  #partnership
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "partnership"),
                          str_ext(title, ".*partnership"),
                          org_name)) %>%
  #partnership
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "union"),
                          title,
                          org_name)) %>%
  #public health
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "public health"),
                          title,
                          org_name)) %>%
  #foundation
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "foundation"),
                          title,
                          org_name)) %>%
  #foundation
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "board of"),
                          str_ext(title, "\\w+ board of.*"),
                          org_name)) %>%
  #university
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "university"),
                          title,
                          org_name)) %>%
  #city of
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "city of"),
                          title,
                          org_name)) %>%
  #city of
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "commerce"),
                          title,
                          org_name)) %>%
  # #names
  # mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ".*, .*president, .*, .*"),
  #                         str_rm(title, ".*, .*president, .*,"),
  #                         org_name)) %>%
  # #names2
  # mutate(org_name= ifelse(is.na(org_name) & str_dct(title, ", .*president,"),
  #                         str_rm(title, ".*, .*president,"),
  #                         org_name)) %>%
  #names3
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "president,"),
                          str_rm(title, ".*president,"),
                          org_name)) %>%
  #testimony from
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "testimony from"),
                          str_rm(title, "testimony from"),
                          org_name)) %>%
  #request for an extension
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "request for extension from"),
                          str_rm(title, "request for extension from"),
                          org_name)) %>%
  #group
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "group"),
                          title,
                          org_name))


#Specific Cases
###############

#docket
d %<>%
  #Western Energy Alliance, The Independent Petroleum Association of America, the American Exploration & Production Council
  mutate(org_name= ifelse(is.na(org_name) & str_dct(document_id, "NPS-2015-0006-0008"),
                          "American Petroleum Institute, Western Energy Alliance, The Independent Petroleum Association of America, the American Exploration & Production Council",
                          org_name)) %>%
  #National Parks Conservation Association, the Natural Resources Defense Council, The Wilderness Society, Sierra Club, Park Rangers for Our Lands, the Ohio Environmental Council
  mutate(org_name= ifelse(is.na(org_name) & str_dct(document_id, "NPS-2015-0006-0015"),
                          "National Parks Conservation Association; the Natural Resources Defense Council; The Wilderness Society; Sierra Club; Park Rangers for Our Lands; the Ohio Environmental Council",
                          org_name)) %>%
  #Trustees for Alaska, National Parks Conservation Association, Denali Citizens Council, Center for Biological Diversity, The Wilderness Society, Defenders of Wildlife, Copper Country Alliance, Northern Alaska Environmental Center, Alaska Center for the Environment, Natural Resources Defense Council, and Audubon Alaska
  mutate(org_name= ifelse(is.na(org_name) & str_dct(document_id, "NPS-2015-0006-0015"),
                          "Trustees for Alaska; National Parks Conservation Association; Denali Citizens Council; Center for Biological Diversity; The Wilderness Society; Defenders of Wildlife; Copper Country Alliance; Northern Alaska Environmental Center; Alaska Center for the Environment; Natural Resources Defense Council; Audubon Alaska",
                          org_name)) %>%
  #Central Sierra Environmental Resource Center
  mutate(org_name= ifelse(is.na(org_name) & str_dct(document_id, "NPS-2014-0004-1150"),
                          "Central Sierra Environmental Resource Center",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(document_id, "NPS-2014-0004-1159"),
                          "Central Sierra Environmental Resource Center",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(document_id, "NPS-2014-0004-1163"),
                          "Central Sierra Environmental Resource Center",
                          org_name)) %>%
  #the state of utah
  mutate(org_name= ifelse(is.na(org_name) & str_dct(document_id, "NPS-2015-0006-0018"),
                          "The State of Utah",
                          org_name))

#title
d %<>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "WildEarth Guardians|WildEarthGuardians"),
                          "WildEarth Guardians",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Care2."),
                          "Care2",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Jane Goodall Institute"),
                          "Jane Goodall Institute",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Humane Society International"),
                          "Humane Society International",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Defenders of Wildlife.*|Defenders of Wildlife"),
                          "Defenders of Wildlife",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "League of Conservation Voters"),
                          "League of Conservation Voters",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Care2"),
                          "Care2",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Environmental Action"),
                          "Environmental Action",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "CascadiaWildlands"),
                          "CascadiaWildlands",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Cascadia Wildlands"),
                          "Cascadia Wildlands",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Sierra Club|SierraClub"),
                          "Sierra Club",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Biological Diversity|,*Center for Biological Diversity"),
                          "Center for Biological Diversity",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Colorado Wolf & Wildlife Center"),
                          "Colorado Wolf & Wildlife Center",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Williams Community Forest Project"),
                          "Williams Community Forest Project",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Binghamton Zoo"),
                          "Binghamton Zoo",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Animal Rescute Site"),
                          "Animal Rescue Site",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Big Game Forever"),
                          "Big Game Forever",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "International Fund For Animal Welfare"),
                          "International Fund For Animal Welfare",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Missourians for a Balanced Energy Future"),
                          "Missourians for a Balanced Energy Future",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "NPCA"),
                          "National Parks Conservation Association",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "PEW"),
                          "Pew Research Center",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "festival foods"),
                          "Festival Foods",
                          org_name)) %>%
  mutate(org_name= ifelse(str_dct(title, "Union of Concerned Scientists"),
                          "union of concerned scientists",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "Kwik Trip"),
                          "Kwik Trip",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "ChangeLab Solutions"),
                          "ChangeLab Solutions",
                          org_name)) %>%
  mutate(org_name= ifelse(is.na(org_name) & str_dct(title, "National Congress of American Indians"),
                          "National Congress of American Indians",
                          org_name)) %>%
  mutate(org_name= ifelse(str_dct(org_name, "Chamber of Commerce"),
                          "Chamber of Commerce",
                          org_name))









################################################################
#text
d %<>%
  #International Fund for Animal Welfare
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "International Fund For Animal Welfare"),
                          "International Fund For Animal Welfare",
                          org_name)) %>%
  #Endangered Species Coalition
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Endangered Species Coalition"),
                          "Endangered Species Coalition",
                          org_name)) %>%
  #Conservation Northwest
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Conservation Northwest"),
                          "Conservation Northwest",
                          org_name)) %>%
  #Save Our Environment, making caps matter because happens a lot in the text
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Save Our Environment"),
                          "Save Our Environment",
                          org_name)) %>%
  #Oregon Wild
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Oregon Wild"),
                          "Oregon Wild",
                          org_name)) %>%
  #Audobon California
  #FIXME
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Audobon California"),
                          "Audubon California",
                          org_name)) %>%
  #Care2
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Care2"),
                          "Care2",
                          org_name)) %>%
  #Sierra Club
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Sierra Club"),
                          "Sierra Club",
                          org_name)) %>%
  #CREDO Action
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "CREDO Action"),
                          "CREDO Action",
                          org_name)) %>%
  #Public Citizen
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Public Citizen"),
                          "Public Citizen",
                          org_name)) %>%
  #no-reply@democracyinaction.org
  #FIXME
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "no-reply@democracyinaction.org"),
                          "Democracy In Action",
                          org_name)) %>%
  #American Lung Association
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "See attached letter from 617 health professionals"),
                          "American Lung Association",
                          org_name)) %>%
  #Power Shift Network
  #FIXME
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "collected by the Power Shift Network"),
                           "Power Shift Network",
                          org_name)) %>%
  #Trustees for Alaska
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "written by Trustees for Alaska"),
                          "Trustees for Alaska",
                          org_name)) %>%
  #National Parks Conservation Association
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "National Parks Conservation Association"),
                          "National Parks Conservation Association",
                          org_name)) %>%
  #Catharsis on the Mall
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "catharsis on the mall"),
                          "Catharsis on the Mall",
                          org_name)) %>%
  #The November Project
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "november project"),
                          "November Project",
                          org_name)) %>%
  #The Board of Directors of the Society for Ethnomusicology
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "The Board of Directors of the Society for Ethnomusicology"),
                          "Society for Ethnomusicology",
                          org_name)) %>%
  #Doyon, Limited
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Doyon, Limited"),
                          "Doyon, Limited",
                          org_name)) %>%
  #Collier Resources Company
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Collier Resources"),
                          "Collier Resources",
                          org_name)) %>%
  #central sierra environmental resource center
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Central Sierra Environmental Resource Center|CSERC"),
                          "Central Sierra Environmental Resource Center",
                          org_name)) %>%
  #human society
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "the humane society"),
                          "Humane Society",
                          org_name)) %>%
  #congressional sportmen's foundation
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Congressional Sportsmen's Foundation"),
                          "Congressional Sportsmen's Foundation",
                          org_name)) %>%
  #Alaska Wilderness League
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "Alaska Wilderness League"),
                          "Alaska Wilderness League",
                          org_name)) %>%
  #institute
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "institute|insitute"),
                          str_ext(comment_text, "(\\w+ )*institute"),
                          org_name)) %>%
  #co.
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, ".*Co\\.$"),
                          str_ext(comment_text, "(\\w+ )*co"),
                          org_name)) %>%
  #corp
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "corp |corp\\."),
                          str_ext(comment_text, "(\\w+ )*corp"),
                          org_name)) %>%
  #Inc.
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, ".*Inc\\..*|.*Inc \\.*"),
                          str_ext(comment_text, "(\\w+ )*inc"),
                          org_name)) %>%
  #LLP
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "LLP"),
                          str_ext(comment_text, "(\\w+ )*llp"),
                          org_name)) %>%
  #LLC
  mutate(org_name= ifelse(is.na(org_name) & str_dct(comment_text, "LLC|LC"),
                          str_ext(comment_text, "(\\w+ )*llc"),
                          org_name))


