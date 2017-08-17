library(mongoplyr)

conn <- mongo(db = "mongoplyr_tests", collection = "restaurants")

MongoPipeline() %>%
	mmatch(.borough == "Manhattan" & .cuisine == "Pizza") %>%
	mexecute(conn) -> pizzaPlacesInManhattan

MongoPipeline() %>%
	mgroup(by = .list(cuisine = .cuisine, borough = .borough), count = .sum(1)) %>%
	mexecute(conn) -> cuisineFrequencyByBorough

boroughs <- c("Manhattan", "Staten Island", "Bronx", "Queens", "Brooklyn")
getRandomNYBorough <- function()
{
	sample(boroughs, 1)
}

getNYBoroughByIndex <- function(i)
{
	boroughs[[i]]
}

MongoPipeline() %>%
	mmatch(.borough == getRandomNYBorough()) %>%
	mexecute(conn) -> restaurantsInRandomBorough

MongoPipeline() %>%
	mmatch(.borough == getNYBoroughByIndex(5)) %>%
	mexecute(conn) -> restaurantsInBrooklynByIndex

input <- list(cuisine = "Continental")
MongoPipeline() %>%
	mmatch(.cuisine == input$cuisine) %>%
	mexecute(conn) -> continentalCuisineFromInput

MongoPipeline() %>%
	munwind(.grades) %>%
	msort(.grades.grade = 1, .grades.date = -1) %>%
	mexecute(conn) -> restaurantRatingsSortedByGradeAndDate
