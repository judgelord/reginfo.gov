# check comments folder
directory <- "comments"
length(list.files(directory))

# load packages
source("setup.R")

## This script downloads pdf attachments only
## The advantage of this approach is that it does not require using the API to get file names
## However, after it is run, one should run regulation-gov-get-attachments.R on the fails to get non-pdfs

load(here("data", "comment_meta_min.Rdata")) #FIXME check for updates

dim(comment_meta_min)
names(comment_meta_min)

d <- comment_meta_min

# urls for first attachments
d %<>%
  mutate(agency_id = str_remove(id, "-.*"),
         year = posted_date %>% str_sub(1,4) %>% as.integer(),
         file = str_c(id, "-1.pdf"),
         attach_url = str_c("https://downloads.regulations.gov/", id, "/attachment_1.pdf"),
         #attach.url = str_c("https://www.regulations.gov/contentStreamer?documentId=", id,"&attachmentNumber=1"),
         downloaded = file %in% list.files(here::here("comments")) ) %>%
  filter(year > 2004,
         !downloaded,
         attachment_count > 0)

# inspect missing files
d %>% count(agency_id, sort = T) %>% head() %>% knitr::kable()
dim(d)
names(d)
head(d)
max(d$year, na.rm = T)

# # MASS DOCKETS:
# load(here("data/masscomments.Rdata"))
# d <- filter(all, docketId %in% mass$docketId)

## Comments from one agency
# d <- filter(all, agencyAcronym %in% c("EPA", "BSEE", "DOJ", "DOD", "ED", "CFPB", "DOL", "MSHA", "DOI", "PHMSA"))

# Inspect
nrow(d)
head(d)

# subset to download now
docs <- d %>% filter(#org.comment, # comments identified as a org comment
                     !is.na(organization) | number_of_comments_received > 1, # comments with an org name identified
                     attachment_count>0) # subset to those with attachment

# Inspect
nrow(docs)
docs %>% head() %>% select(file, attach_url)


docs %>%  count(agency_id, sort = T) %>% knitr::kable()
#################################
# files we do have
# downloaded <- filter(docs, downloaded)

# inspect
# dim(downloaded)
#head(downloaded)
#sum(downloaded$number_of_comments_received)
#write.table(sum(downloaded$number_of_comments_received), file = "data/downloaded.tex")

# to DOWNLOAD
# files we don't have
download <- filter(docs,
                     !downloaded,
                     !attach_url %in% c("","NULL"),
                     !is.na(attach_url),
                     !is.null(attach_url) )

# inspect
dim(docs)
dim(download)
download %>% head() %>% select(file, attach_url)
download %>%  count(agency_id, sort = T) %>% head() %>% knitr::kable()

# download %<>% filter(agency_id %in% c("ATF"))
dim(download)
# Load data on failed downloads
load("data/comment_fails.Rdata")

# drop files that we have already tried and failed to download
download %<>% anti_join(fails)

# inspect
dim(download)
head(download$attach_url)


download %<>% filter( !is.na(file) )

download %<>% filter(!file %in% list.files("comments/") )

# inspect
dim(download)
head(download$attach_url)

# remove large data files from  memory
rm(list("comment_meta_min"))

# test
n <- 1
for(i in 1:n){
download.file(download$attach_url[i],
              destfile = str_c("comments/", download$file[i]) )
}

Sys.sleep(400) # wait after downloading the first one so we don't hit the limit on the first loop


# loop over downloading attachments 78 at a time (regulations.gov blocks after 78?)
# FIXME should use purrr walk() here and in get-attachments
# for(attachment_n in 1:max(download$attachmentCount)){} # loop to capture multiple attachments, starting with first file for all with attachmentCoung > 0, then subsetting to attachmentCount > 1, etc
N <- nrow(download)
batch <- 178

# run this loop about N/batch size times to get everything
for(i in 1:round(nrow(download)/batch)){
  # n complete
  n <- (i-1)*batch
  # filter out files already downloaded
  download %<>% filter(!file %in% fails$file,
                       !file %in% list.files("comments/") )

  ## Reset error counter
  errorcount <<- 0

  for(i in 1:batch){
    message(paste(n+i, "of", N, download$file[i], "at", Sys.time()))

    if(errorcount < 50 & nrow(download) > 0){
      # download to comments folder
      tryCatch({ # tryCatch handles errors
        download.file(download$attach_url[i],
                      destfile = paste0("comments/", download$file[i]) )
      },
      error = function(e) {
        errorcount <<- errorcount+1
        message(paste("error", errorcount))
        if( str_detect(e[[1]][1], "cannot open URL") ){
          fails <<- rbind(fails, download$file[i])
        }
        message(e)
        if(str_detect(e, "SSL connect error|500 Internal Server Error")){
          # beepr::beep()
          Sys.sleep(600) # wait 10
          errorcount <<- 0 # reset error counter
        }
      })

      Sys.sleep(.1) # easier to stop failed calls
    }
    ## If 5 errors, wait and reset (sometimes you get "cannot open URL" 5x in a row)
    if(errorcount == 50){
      message("--paused after 5 errors")
      # beepr::beep()
      Sys.sleep(600) # wait 10 min
      errorcount <<- 0 # reset error counter
    }

    } # end loop over batch


  # Save data on failed downloads
  save(fails, file = "data/comment_fails.Rdata")
  message("Pausing to prevent regulations.gov from blocking this IP address")
  Sys.sleep(400) # wait 400 seconds (300 is not enough)
} # end main loop

# If you get blocked (SSL connect error), you need to wait longer than 10 min, 30 min seems about right







