import::here(readxl, read_excel)
import::here(tidyr, gather)
import::here(dplyr, .all = TRUE)
import::here(purrr, map)
import::here(magrittr, "%>%")

capacityTbl <-
  read_excel("Chap Preferences.xlsx", sheet = 2) %>%
  select(event = Outing, capacity = `Number of People Needed`, points = Points) %>%
  mutate(id = row_number())

req_points <- read_excel("Chap Preferences.xlsx", sheet = 3)

mergeData <-
  read_excel("Chap Preferences.xlsx", sheet = 1) %>%
  rename(name = Name) %>%
  arrange(name) %>%
  mutate(i = row_number()) %>%
  gather("preference", "event", -name, -i) %>%
  mutate(
    preference =
      case_when(
        preference == "Please select your top preference from the drop-down list." ~ 1,
        preference == "Second preference" ~ 2,
        preference == "Third Preference" ~ 3,
        preference == "Fourth Preference" ~ 4,
        preference == "Fifth Preference" ~ 5,
        preference == "Sixth Preference" ~ 6
      )
  ) %>%
inner_join(capacityTbl, by = "event") %>%
inner_join(req_points, by = "name") %>%
arrange(name, preference)

preferenceData <- mergeData %>% group_by(name) %>% group_split() %>% map(~{.x$id})

capacity <- capacityTbl %>% arrange(id) %>% pull(capacity)
