library(mongoplyr)

conn <- mongo(db = "mongoplyr_tests", collection = "restaurants")

MongoPipeline() %>%
	match(.borough == "Manhattan" & .cuisine == "Pizza") %>%
	execute(conn) -> pizzaPlacesInManhattan

MongoPipeline() %>%
	group(by = .list(cuisine = .cuisine, borough = .borough), count = .sum(1)) %>%
	execute(conn) -> cuisineFrequencyByBorough


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
	match(.borough == getRandomNYBorough()) %>%
	execute(conn) -> restaurantsInRandomBorough

MongoPipeline() %>%
	match(.borough == getNYBoroughByIndex(5)) %>%
	execute(conn) -> restaurantsInBrooklynByIndex

input <- list(cuisine = "Continental")
MongoPipeline() %>%
	match(.cuisine == input$cuisine) %>%
	execute(conn) -> continentalCuisineFromInput

MongoPipeline() %>%
	unwind(.grades) %>%
	sort(.grades.grade = 1, .grades.date = -1) %>%
	execute(conn) -> restaurantRatingsSortedByGradeAndDate
