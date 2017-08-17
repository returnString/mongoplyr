library(mongoplyr)

conn <- mongo(db = "mongoplyr_tests", collection = "restaurants")

# simple example of filtering by certain criteria
MongoPipeline() %>%
	mmatch(.borough == "Manhattan" & .cuisine == "Pizza") %>%
	mexecute(conn) -> pizzaPlacesInManhattan

# grouping by a number of variables, and counting the frequencies
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

# you can use normal R functions in your query expressions
# because functions are executed pre-query, they can't reference mongo fields
MongoPipeline() %>%
	mmatch(.borough == getRandomNYBorough()) %>%
	mexecute(conn) -> restaurantsInRandomBorough

# as above, but just ensuring parameterised functions work too
MongoPipeline() %>%
	mmatch(.borough == getNYBoroughByIndex(5)) %>%
	mexecute(conn) -> restaurantsInBrooklynByIndex

# often in Shiny dashboards, you have an `input` var containing all your UI options
# this example demonstrates that $ referencing works, with the same rules as functions
input <- list(cuisine = "Continental")
MongoPipeline() %>%
	mmatch(.cuisine == input$cuisine) %>%
	mexecute(conn) -> continentalCuisineFromInput

# unwinding creates a new document set based on values from a sub-array in all your documents
# here, we create a 'row' for every grade a restaurant ever received
# and then sort them by both actual grade, and the date it was graded
MongoPipeline() %>%
	munwind(.grades) %>%
	msort(.grades.grade = 1, .grades.date = -1) %>%
	mexecute(conn) -> restaurantRatingsSortedByGradeAndDate
