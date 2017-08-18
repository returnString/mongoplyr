context("Unwind")

test_that("unwind maps to $unwind stage",
{
	MongoPipeline() %>% munwind(.nestedArray) %>%
		pipelineAssert('{"$unwind":"$nestedArray"}')
})
