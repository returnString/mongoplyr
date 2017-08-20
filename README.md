# mongoplyr

[![Travis-CI Build Status](https://travis-ci.org/returnString/mongoplyr.svg?branch=master)](https://travis-ci.org/returnString/mongoplyr)
[![Coverage Status](https://coveralls.io/repos/github/returnString/mongoplyr/badge.svg?branch=master)](https://coveralls.io/github/returnString/mongoplyr?branch=master)

A native R interface to the MongoDB aggregation pipeline.

Inspired by the pipeline-esque syntax enjoyed when doing in-memory work with a combination of `dplyr` and `magrittr`, mongoplyr allows you to construct aggregation pipeline queries for MongoDB without the ugliness and insecurity of pasting strings together.

It uses [jeroen/mongolite](https://github.com/jeroen/mongolite) as a client library to actually communicate with MongoDB servers.

If you have a feature request, e.g. to prioritise an unimplemented portion of the aggregation pipeline, or a bug, please either file an issue on GitHub or poke me on [Twitter](https://twitter.com/returnString).

# Example usage
```r
library(mongoplyr)

# using the MongoDB NY restaurant dataset
conn <- mongo(db = "mydb", collection = "restaurants")

MongoPipeline() %>%
	mmatch(.borough == "Manhattan") %>%
	mgroup(by = .cuisine, count = .sum(1)) %>%
	msort(.count = -1) %>%
	mlimit(5) %>%
	mexecute(conn) -> topFiveCuisinesInManhattan
```

Should result in a data frame like so:
```
                id  count
1         American  3205
2  Café/Coffee/Tea   680
3          Italian   621
4          Chinese   510
5         Japanese   438
```

# Installation
The package is not yet available on CRAN. However, it can be installed directly from source with the `devtools` package:

```r
devtools::install_github("returnString/mongoplyr")
```

Currently supported and tested R versions:
- 3.4.x
- 3.3.x

This package is also built and tested against the latest development build of R.

With regards to platforms, we officially test all changes on Linux with Travis CI, and most development currently occurs on Windows. I aim to implement automated testing on Windows and OSX to expand this.

# Production usage
This package is currently in use at Deep Silver Dambuster Studios, primarily inside of Shiny dashboards for internal cluster telemetry, where we extract and prepare data for further analysis and presentation. This has caused a 99% reduction in our usage of `paste0` ;)

That said, due to the relative immaturity of this codebase, please exercise caution and verify your results independently from this package before committing to presenting data.
