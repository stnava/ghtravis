library(httr)
library(tidyr)
library(dplyr)

# file = "https://s3.amazonaws.com/archive.travis-ci.org/jobs/269241330/log.txt?X-Amz-Expires=30&X-Amz-Date=20170828T163440Z&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAJRYRXRSVGNKPKO5A/20170828/us-east-1/s3/aws4_request&X-Amz-SignedHeaders=host&X-Amz-Signature=b24d4775d8ea506c604b1b20d934e53eba4954e1cf71abbcda1e2cf251176208"
job_id = "269241330"
file = paste0("https://api.travis-ci.org/jobs/",
              job_id,
              "/log.txt?deansi=true")
res = httr::GET(file)
x = content(res)

log = unlist(strsplit(x, "\n"))
log = unlist(strsplit(log, "\r"))
log = trimws(log)
log = log[ !log %in% "" ]
#remove front non-ASCII
travis_terms = "travis_[time|fold]"
log = sub(
  paste0(".*(", travis_terms, ".*)"),
  "\\1", log)

timings = grep(paste0("^", travis_terms),
               log, value = TRUE)

df = data_frame(timing = timings)
df = df %>%
  separate(
    timing,
    into = c("travis_type", "type", "id", "info"),
    sep = ":")
df$travis_type = sub("travis_", "", df$travis_type)

id_df = df %>%
  select(id) %>%
  distinct()
id_df$ord = seq.int(nrow(id_df))

df = df %>%
  spread(key = type, value = info)
df$start = NULL
df = df %>%
  separate(
    end,
    into = c("start", "finish", "duration"),
    sep = ",")
remove_equals = function(x) {
  x = sub(".*=(.*)", "\\1", x)
  x = as.numeric(x)
}
df = df %>%
  mutate(start = remove_equals(start),
         finish = remove_equals(finish),
         duration = remove_equals(duration)
  )
df = left_join(df, id_df)
df = df %>%
  arrange(ord)

