\name{RedditExtractoR}
\alias{RedditExtractoR}
\alias{RedditExtractoR}
\docType{package}
\title{
Reddit Data Extraction Toolkit
}
\description{
Reddit is an online bulletin board and a social networking website where registered users can submit and discuss content. This package uses Reddit API to retrieve comments together will all corresponding attributes from Reddit threads. Note that at this stage, the extraction produces a data frame with a flat structure, i.e. without preserving the order or heirarchy of individuals comments. This may be addressed in the next version of this package. Also note that due to API limitations, the number of comments available for retrieval is limited to 500 per thread.
}
\details{
\tabular{ll}{
Package: \tab RedditExtractoR\cr
Type: \tab Package\cr
Version: \tab 1.1.1\cr
Date: \tab 2015-06-14\cr
License: \tab GPL-3 \cr
}
The package contains 3 functions, ObtainRedditURLs which extracts URLs based on a search query, reddit_content which extracts URL attributes or features, and get_reddit which servers as a wrapper for the first 2. It is however recommended to use reddit_urls and reddit_content which will help you refine your query.
}
\author{
Ivan Rivera

Maintainer: Ivan Rivera <ivan.s.rivera@gmail.com>
}
\references{
\url{https://www.reddit.com/dev/api}
}
\keyword{reddit}
\seealso{
\url{www.reddit.com}
}
\examples{
example_urls = reddit_urls(search_terms="science")
\dontrun{
example_attr = reddit_content(URL="reddit.com/r/gifs/comments/39tzsy/whale_watching")
example_data = get_reddit(search_terms="economy")}
}
