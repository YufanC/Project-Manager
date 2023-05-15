library(dplyr)

calendar <- read.csv("Yufan Chen Calendar.csv")

calendar1 <- calendar %>% 
  mutate(start_time = format(strptime(Start.Time, "%I:%M:%S %p"), "%H:%M:%S"),
         end_time = format(strptime(End.Time, "%I:%M:%S %p"), "%H:%M:%S"),
         start_date = as.character(as.Date.character(Start.Date, format = "%m/%d/%Y")),
         end_date = as.Date.character(End.Date, format = "%m/%d/%Y"),
         start = paste(start_date, trimws(start_time), " "),
         end = paste(end_date, trimws(end_time), " "),
         title = `ï..Subject`,
         group = 1,
         editable = TRUE,
         style = NA) %>% 
  rename(content = `ï..Subject`) %>% 
  bind_cols(id = paste0("item", 1:nrow(calendar))) %>% 
  select(id, content, title, start_date, start, end, group, editable, style)

write.csv(calendar1, file = "Yufan Chen Calendar1.csv", row.names = FALSE, quote = TRUE)

calendar[, grepl("Subject", colnames(calendar))]
