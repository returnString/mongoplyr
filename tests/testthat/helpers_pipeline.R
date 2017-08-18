pipelineAssert <- function(p, ...)
{
	stageStrings <- as.character(...)
	expect_equal(p@stages, stageStrings)
}
