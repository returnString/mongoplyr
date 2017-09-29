context("Match")

test_that("single-field equality matches use $eq",
{
	MongoPipeline() %>% mmatch(.field == "value") %>%
		pipelineAssert('{"$match":{"field":{"$eq":"value"}}}')
})

test_that("multiple-field equality matches use $and/$eq",
{
	MongoPipeline() %>% mmatch(.field1 == "value" & .field2 == 1) %>%
		pipelineAssert('{"$match":{"$and":[{"field1":{"$eq":"value"}},{"field2":{"$eq":1}}]}}')
})

test_that("or maps to $or/$eq",
{
	MongoPipeline() %>% mmatch(.field1 == "value" | .field2 == 1) %>%
		pipelineAssert('{"$match":{"$or":[{"field1":{"$eq":"value"}},{"field2":{"$eq":1}}]}}')
})

test_that("!= maps to $ne",
{
	MongoPipeline() %>% mmatch(.field != "value to exclude") %>%
		pipelineAssert('{"$match":{"field":{"$ne":"value to exclude"}}}')
})

test_that("numeric operators use $gl(e)/$lt(e)",
{
	MongoPipeline() %>% mmatch(.timestamp > 100 & .timestamp < 1000) %>%
		pipelineAssert('{"$match":{"$and":[{"timestamp":{"$gt":100}},{"timestamp":{"$lt":1000}}]}}')

	MongoPipeline() %>% mmatch(.timestamp >= 100 & .timestamp <= 1000) %>%
		pipelineAssert('{"$match":{"$and":[{"timestamp":{"$gte":100}},{"timestamp":{"$lte":1000}}]}}')
})

test_that("R's %in% operator is mapped to $in",
{
	MongoPipeline() %>% mmatch(.field %in% c("one option", "another option")) %>%
		pipelineAssert('{"$match":{"field":{"$in":["one option","another option"]}}}')

	MongoPipeline() %>% mmatch(.field %in% c("one option")) %>%
		pipelineAssert('{"$match":{"field":{"$in":["one option"]}}}')
})
