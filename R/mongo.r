mongoPrimitiveOps = c("&"
	, "|"
	, "=="
	, "!="
	, ">"
	, ">="
	, "<"
	, "<="
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

				if (startsWith(value, "."))
				{
					value <- sub("^\\.", ifelse(dollarForMemberAccess, "$", ""), value)
					return(list(type = "member", value = value))
				}
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

				if (opText %in% mongoPrimitiveOps | startsWith(opText, "."))
				{
					args <- lapply(x[2:length(x)], visit)
					opText <- sub("^\\.", "", opText)
					return(list(type = "call", op = opText, args = args))
				}
				else
				{
					rargs <- list()
					if (length(x) > 1)
					{
						rargs <- as.list(x[2:length(x)])
					}

					result <- do.call(op$value, rargs)
					return(list(type = "atomic", value = result))
				}
			}
			else
			{
				stop(paste0("unhandled type: ", typeof(x)))
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
				"&" = arrayExpr("and"),
				"|" = arrayExpr("or"),
				"==" = binaryExpr("eq"),
				"!=" = binaryExpr("ne"),
				">" = binaryExpr("gt"),
				">=" = binaryExpr("gte"),
				"<" = binaryExpr("lt"),
				"<=" = binaryExpr("lte"),
				"sum" = renderFunc,
				"first" = renderFunc,
				"list" = renderSubobject,
				stop(paste("unhandled operator:", node$op))
			)

			func(node)
		}
		else if (node$type == "atomic" || node$type == "member")
		{
			jsonlite::unbox(node$value)
		}
		else
		{
			stop(paste("unhandled type:", node$type))
		}
	}

	visit(ast)
}

createStage <- function(name, payload)
{
	ret <- list()
	ret[[paste0("$", name)]] <- payload
	jsonlite::toJSON(ret)
}

#' @export MongoPipeline
MongoPipeline <- setClass("MongoPipeline", slots = c(stages = "character"))

#' @export
setGeneric("match", function(p, expr) standardGeneric("match"))
setMethod("match", signature(p = "MongoPipeline"),
	function(p, expr)
	{
		parser <- exprParser()
		ast <- parser(substitute(expr))
		stage <- createStage("match", mongoAstToList(ast))
		p@stages <- c(p@stages, stage)
		p
	})

#' @export
setGeneric("limit", function(p, limit) standardGeneric("limit"))
setMethod("limit", signature(p = "MongoPipeline", limit = "numeric"),
	function(p, limit)
	{
		p@stages <- c(p@stages, createStage("limit", jsonlite::unbox(limit)))
		p
	})

#' @export
setGeneric("group", function(p, by, ...) standardGeneric("group"))
setMethod("group", signature(p = "MongoPipeline"),
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

#' @export
setGeneric("unwind", function(p, expr) standardGeneric("unwind"))
setMethod("unwind", signature(p = "MongoPipeline"),
	function(p, expr)
	{
		parser <- exprParser()
		arg <- parser(substitute(expr), dollarForMemberAccess = T)
		p@stages <- c(p@stages, createStage("unwind", mongoAstToList(arg)))
		p
	})

#' @export
setGeneric("sort", function(p, ...) standardGeneric("sort"))
setMethod("sort", signature(p = "MongoPipeline"),
	function(p, ...)
	{
		namedArgs <- lapply(list(...), jsonlite::unbox)
		names(namedArgs) <- sapply(names(namedArgs), function(n) sub("^\\.", "", n))
		p@stages <- c(p@stages, createStage("sort", namedArgs))
		p
	})

# TODO: need to figure out how to dispatch specifically on a mongolite client
#' @export
setGeneric("execute", function(p, ...) standardGeneric("execute"))
setMethod("execute", signature(p = "MongoPipeline"),
	function(p, client)
	{
		pipeline <- paste0("[", toString(p@stages), "]")
		flattenedFrame <- jsonlite::flatten(client$aggregate(pipeline))
		colnames(flattenedFrame) <- gsub("^_id", "id", colnames(flattenedFrame))
		flattenedFrame
	})
