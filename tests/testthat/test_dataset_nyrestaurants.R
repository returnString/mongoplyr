context("Reference restaurant dataset")

conn <- mongo(db = "mongoplyr_tests", collection = "restaurants")

test_that("execute works with a multistep pipeline",
{
	MongoPipeline() %>%
		mmatch(.borough == "Manhattan") %>%
		mgroup(by = .cuisine, count = .sum(1)) %>%
		msort(.count = -1) %>%
		mlimit(5) %>%
		mexecute(conn) -> data

	expect_equal(iconv(data$id, to = "UTF-8"), c("American", "Café/Coffee/Tea", "Italian", "Chinese", "Japanese"))
	expect_equal(data$count, c(3205, 680, 621, 510, 438))
})
