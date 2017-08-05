library(mongoplyr)

conn <- mongo(db = "mongoplyr_tests", collection = "restaurants")

MongoPipeline() %>%
  match(.borough == "Manhattan" & .cuisine == "Pizza") %>%
  execute(conn) -> pizzaPlacesInManhattan

MongoPipeline() %>%
  group(by = list(cuisine = .cuisine, borough = .borough), count = sum(1)) %>%
  execute(conn) -> cuisineFrequencyByBorough
