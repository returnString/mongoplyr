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
				"+" = arrayExpr("add"),
				"substr" = arrayExpr("substr"),
				"sum" = renderFunc,
				"min" = renderFunc,
				"max" = renderFunc,
				"first" = renderFunc,
				"list" = renderSubobject,
				"%in%" = binaryExpr("in"),
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
			stop(paste("unhandled type:", node$type))
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

#' @export MongoPipeline
MongoPipeline <- setClass("MongoPipeline", slots = c(stages = "character"))

#' @export
setGeneric("mmatch", function(p, expr) standardGeneric("mmatch"))
setMethod("mmatch", signature(p = "MongoPipeline"),
	function(p, expr)
	{
		parser <- exprParser()
		ast <- parser(substitute(expr))
		stage <- createStage("match", mongoAstToList(ast))
		p@stages <- c(p@stages, stage)
		p
	})

#' @export
setGeneric("mlimit", function(p, limit) standardGeneric("mlimit"))
setMethod("mlimit", signature(p = "MongoPipeline", limit = "numeric"),
	function(p, limit)
	{
		p@stages <- c(p@stages, createStage("limit", jsonlite::unbox(limit)))
		p
	})

#' @export
setGeneric("mgroup", function(p, by, ...) standardGeneric("mgroup"))
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

#' @export
setGeneric("munwind", function(p, expr) standardGeneric("munwind"))
setMethod("munwind", signature(p = "MongoPipeline"),
	function(p, expr)
	{
		parser <- exprParser()
		arg <- parser(substitute(expr), dollarForMemberAccess = T)
		p@stages <- c(p@stages, createStage("unwind", mongoAstToList(arg)))
		p
	})

#' @export
setGeneric("msort", function(p, ...) standardGeneric("msort"))
setMethod("msort", signature(p = "MongoPipeline"),
	function(p, ...)
	{
		namedArgs <- lapply(list(...), jsonlite::unbox)
		names(namedArgs) <- sapply(names(namedArgs), function(n) sub("^\\.", "", n))
		p@stages <- c(p@stages, createStage("sort", namedArgs))
		p
	})

# TODO: need to figure out how to dispatch specifically on a mongolite client
#' @export
setGeneric("mexecute", function(p, ...) standardGeneric("mexecute"))
setMethod("mexecute", signature(p = "MongoPipeline"),
	function(p, client)
	{
		pipeline <- paste0("[", toString(p@stages), "]")
		flattenedFrame <- jsonlite::flatten(client$aggregate(pipeline))
		colnames(flattenedFrame) <- gsub("^_id", "id", colnames(flattenedFrame))
		flattenedFrame
	})