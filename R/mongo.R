mongoPrimitiveOps = c("&"
	, "|"
	, "=="
	, "!="
	, ">"
	, ">="
	, "<"
	, "<="
	, "%in%"
	, "+"
)

exprParser <- function()
{
	callingFrame <- parent.frame(2)

	function(expr, dollarForMemberAccess = F)
	{
		visit <- function(x)
		{
			if (is.atomic(x))
			{
				return(list(type = "atomic", value = x))
			}
			else if (is.name(x))
			{
				value = deparse(x)

				# period prefix indicates a mongo field (some contexts require the $ prefix)
				if (startsWith(value, "."))
				{
					value <- sub("^\\.", ifelse(dollarForMemberAccess, "$", ""), value)
					return(list(type = "member", value = value))
				}
				# otherwise, it's an R object, so add that actual value to the ast
				else
				{
					obj <- get(value, envir = callingFrame)
					return(list(type = "atomic", value = obj))
				}
			}
			else if (is.call(x))
			{
				op <- visit(x[[1]])
				opText <- deparse(x[[1]])

				# if it's a primitive op, or we're just proxying to a mongo expression,
				# then create an ast node to render that out later
				if (opText %in% mongoPrimitiveOps | startsWith(opText, "."))
				{
					args <- lapply(x[2:length(x)], visit)
					opText <- sub("^\\.", "", opText)
					return(list(type = "call", op = opText, args = args))
				}
				# otherwise, it's a proper R function call (and so contains no mongo field/func refs)
				# so resolve the return value pre-query and sub that in as an atomic ast node
				else
				{
					rargs <- list()
					if (length(x) > 1)
					{
						rargs <- as.list(x[2:length(x)])
					}

					result <- do.call(op$value, rargs, envir = callingFrame)
					return(list(type = "atomic", value = result))
				}
			}
			else
			{
				stop(paste0("unhandled type: ", typeof(x))) # nocov
			}
		}

		visit(expr)
	}
}


mongoAstToList <- function(ast)
{
	visit <- function(node)
	{
		arrayExpr <- function(name)
		{
			function(node)
			{
				ret <- list()
				ret[[paste0("$", name)]] <- lapply(node$args, visit)
				ret
			}
		}

		binaryExpr <- function(name)
		{
			function(node)
			{
				argList <- list()
				argList[[paste0("$", name)]] <- visit(node$args[[2]])

				ret <- list()
				field <- visit(node$args[[1]])
				ret[[field]] <- argList
				ret
			}
		}

		renderFunc <- function(node)
		{
			ret <- list()
			ret[[paste0("$", node$op)]] <- visit(node$args[[1]])
			ret
		}

		renderSubobject <- function(node)
		{
			lapply(node$args, visit)
		}

		if (node$type == "call")
		{
			func <- switch(node$op,

				# expressions
				"&" = { arrayExpr("and") },
				"|" = { arrayExpr("or") },
				"==" = { binaryExpr("eq") },
				"!=" = { binaryExpr("ne") },
				">" = { binaryExpr("gt") },
				">=" = { binaryExpr("gte") },
				"<" = { binaryExpr("lt") },
				"<=" = { binaryExpr("lte") },
				"+" = { arrayExpr("add") },
				"substr" = { arrayExpr("substr") },

				# accumulators
				"sum" = { renderFunc },
				"min" = { renderFunc },
				"max" = { renderFunc },
				"first" = { renderFunc },
				"list" = { renderSubobject },

				stop(paste("unhandled operator:", node$op))
			)

			func(node)
		}
		else if (node$type == "atomic" || node$type == "member")
		{
			node$value
		}
		else
		{
			stop(paste("unhandled type:", node$type)) # nocov
		}
	}

	visit(ast)
}

createStage <- function(name, payload)
{
	ret <- list()
	ret[[paste0("$", name)]] <- payload
	jsonlite::toJSON(ret, auto_unbox = T)
}

#' Represents a MongoDB aggregation pipeline query.
#' @export MongoPipeline
MongoPipeline <- setClass("MongoPipeline", slots = c(stages = "character"))

#' Add a $match stage to a MongoPipeline.
#' @param p A \linkS4class{MongoPipeline} instance.
#' @param expr An expression indicating the requirements to match against.
#' @return A copy of the previous pipeline, with the new stage added.
#' @rdname mmatch-methods
#' @export
setGeneric("mmatch", function(p, expr) standardGeneric("mmatch"))

#' @rdname mmatch-methods
#' @aliases mmatch,MongoPipeline-method
setMethod("mmatch", signature(p = "MongoPipeline"),
	function(p, expr)
	{
		parser <- exprParser()
		ast <- parser(substitute(expr))
		stage <- createStage("match", mongoAstToList(ast))
		p@stages <- c(p@stages, stage)
		p
	})

#' Add a $limit stage to a MongoPipeline.
#' @param p A \linkS4class{MongoPipeline} instance.
#' @param limit The number of documents to include in the result.
#' @return A copy of the previous pipeline, with the new stage added.
#' @rdname mlimit-methods
#' @export
setGeneric("mlimit", function(p, limit) standardGeneric("mlimit"))

#' @rdname mlimit-methods
#' @aliases mlimit,MongoPipeline,numeric-method
setMethod("mlimit", signature(p = "MongoPipeline", limit = "numeric"),
	function(p, limit)
	{
		p@stages <- c(p@stages, createStage("limit", jsonlite::unbox(limit)))
		p
	})

#' Add a $group stage to a MongoPipeline.
#' @param p A \linkS4class{MongoPipeline} instance.
#' @param by The grouping variable(s), as an expression.
#' @param ... Zero or more name=value pairs where name represent
#' a field to create, and values are accumulator expressions.
#' @return A copy of the previous pipeline, with the new stage added.
#' @rdname mgroup-methods
#' @export
setGeneric("mgroup", function(p, by, ...) standardGeneric("mgroup"))

#' @rdname mgroup-methods
#' @aliases mgroup,MongoPipeline-method
setMethod("mgroup", signature(p = "MongoPipeline"),
	function(p, by, ...)
	{
		subbedArgs <- as.list(substitute(list(...)))[-1L]
		subbedArgs[["_id"]] = substitute(by)

		parser <- exprParser()
		asts <- lapply(subbedArgs, function(a) { parser(a, dollarForMemberAccess = T) })
		argList <- lapply(asts, mongoAstToList)

		p@stages <- c(p@stages, createStage("group", argList))
		p
	})

#' Add an $unwind stage to a MongoPipeline.
#' @param p A \linkS4class{MongoPipeline} instance.
#' @param expr An expression indicating the array field to unwind.
#' @return A copy of the previous pipeline, with the new stage added.
#' @rdname munwind-methods
#' @export
setGeneric("munwind", function(p, expr) standardGeneric("munwind"))

#' @rdname munwind-methods
#' @aliases munwind,MongoPipeline-method
setMethod("munwind", signature(p = "MongoPipeline"),
	function(p, expr)
	{
		parser <- exprParser()
		arg <- parser(substitute(expr), dollarForMemberAccess = T)
		p@stages <- c(p@stages, createStage("unwind", mongoAstToList(arg)))
		p
	})

#' Add a $sort stage to a MongoPipeline.
#' @param p A \linkS4class{MongoPipeline} instance.
#' @param ... Named arguments for sort fields, indicating whether to sort in ascending (1) or descending (-1) order.
#' @return A copy of the previous pipeline, with the new stage added.
#' @rdname msort-methods
#' @export
setGeneric("msort", function(p, ...) standardGeneric("msort"))

#' @rdname msort-methods
#' @aliases msort,MongoPipeline-method
setMethod("msort", signature(p = "MongoPipeline"),
	function(p, ...)
	{
		namedArgs <- lapply(list(...), jsonlite::unbox)
		names(namedArgs) <- sapply(names(namedArgs), function(n) sub("^\\.", "", n))
		p@stages <- c(p@stages, createStage("sort", namedArgs))
		p
	})

#' Execute a MongoPipeline's current query.
#' @param p A \linkS4class{MongoPipeline} instance.
#' @param client A connection to the mongo collection to execute against.
#' @return A data frame containing the aggregation pipeline results.
#' @rdname mexecute-methods
#' @export
setGeneric("mexecute", function(p, client) standardGeneric("mexecute"))

#' @rdname mexecute-methods
#' @aliases mexecute,MongoPipeline-method
setMethod("mexecute", signature(p = "MongoPipeline"),
	function(p, client)
	{
		pipeline <- paste0("[", toString(p@stages), "]")
		frame <- client$aggregate(pipeline)
		flattenedFrame <- jsonlite::flatten(frame)
		colnames(flattenedFrame) <- gsub("^_id", "id", colnames(flattenedFrame))
		flattenedFrame
	})
