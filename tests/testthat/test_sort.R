context("Sort")

test_that("sort maps to $sort stage",
{
	MongoPipeline() %>% msort(.field = 1) %>%
		pipelineAssert('{"$sort":{"field":1}}')
})

test_that("sort supports multiple entries in $sort stage",
{
	MongoPipeline() %>% msort(.firstField = 1, .secondField = -1) %>%
		pipelineAssert('{"$sort":{"firstField":1,"secondField":-1}}')
})
