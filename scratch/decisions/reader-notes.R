source("R/caseflowConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")

cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

# need to associate documents with a case

notes <- cfQuery("
  select
    document_id,
    comment,
    category_procedural,
    category_medical,
    category_other,
    user_id

  from annotations

  join documents on document_id = documents.id
")

notes$length <- nchar(notes$comment)

by_document <- notes %>%
  group_by(document_id, user_id, category_procedural, category_medical, category_other) %>%
  tally() %>%
  replace_na(list(category_procedural = FALSE, category_medical = FALSE, category_other = FALSE))

procedural_documents <- by_document %>%
  filter(category_procedural)

mean(procedural_documents$n)
summary(procedural_documents$n)
quantile()

medical_documents <- by_document %>%
  filter(category_medical)

mean(medical_documents$n)
summary(medical_documents$n)

other_documents <- by_document %>%
  filter(category_other)

mean(other_documents$n)
summary(other_documents$n)


cfQuery("select tag_id, text from documents_tags join tags on tag_id = tags.id where document_id = '135560'")
cfQuery("select comment from annotations where document_id = '135560'")$comment %>% paste()
cfQuery("select * from users where id = 472")

tags <- cfQuery("select text, count(*) from documents_tags join tags on tag_id = tags.id group by text") %>%
  filter(count >= 5) %>%
  arrange(desc(count))

write.csv(tags, "tags.csv", row.names = FALSE)


summary(nchar(notes$comment))

by_user <- by_document %>%
  group_by(user_id) %>%
  summarize(mean_n = mean(n))

