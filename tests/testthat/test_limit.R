context("Limit")

test_that("limit maps to $limit stage",
{
	MongoPipeline() %>% mlimit(1) %>%
		pipelineAssert('{"$limit":1}')
})
