#' Retrieve a current list of the top level domains
#' 
#' This function will reach out to \url{https://publicsuffix.org/list/effective_tld_names.dat}, 
#' retrieve the contents and create a simple vector of all the top level domains (removing
#' comments and blanks lines from the file).  
#' 
#' If there is no network connectivity, a cached version of the data ("tldnames") is included with
#' this package and can be loaded with \code{data("tldnames")} after loading this package.
#' 
#' @param url URL of the location for the tld name authority
#' @import httr
#' @export
#' @examples
#' \dontrun{
#' tldnames <- getTLD()
#' }
getTLD <- function(url="https://publicsuffix.org/list/effective_tld_names.dat") {
  rawhttp <- GET(url)
  content <- unlist(strsplit(rawToChar(rawhttp[["content"]]), "\n+"))
  tldnames <- content[grep("^//", content, invert=T)]
  Encoding(tldnames) <- "latin1"
  tldnames <- iconv(tldnames, "latin1", "UTF-8")
  tldnames  
}

#' Extract the top level domain, domain and subdomain from a host name
#' 
#' Given one or more host names, this will return a data frame with four column:
#' \itemize{
#' \item "host": the original host name
#' \item "tld": the top level domain extracted
#' \item "domain" the domain extracted
#' \item "subdomain" one or more subdomains prepending the domain
#' }
#' 
#' If a hostname is not understandable (no top level domain is matched), it will 
#' return NA for the three components.
#' 
#' @param host vector of one or more host names
#' @param tldnames vector of TLD names (see \code{\link{getTLD}})
#' @import data.table
#' @export
#' @seealso getTLD
#' @examples
#' \dontrun{
#' hosts <- tldextract(c("www.google.com", "testing.co.uk"), tldnames=getTLD())
#' }
tldextract <- function(host, tldnames=NULL) {
  if (missing(tldnames)) {
    data("tldnames", envir = environment())
  }
  wilds <- grepl('^\\*', tldnames)
  wildcard <- sub('\\*\\.', "", tldnames[wilds]) 
  static <- tldnames[!wilds]
  
  subdomain <- domain <- tld <- rep(NA_character_, length(host))
  splithosts <- strsplit(host, "[.]")
  names(splithosts) <- seq(length(splithosts))
  maxlen <- max(sapply(splithosts, length))
  for(split.after in seq(1, maxlen-1)) {
    templ <- sapply(splithosts, function(x)
      paste0(x[(split.after+1):length(x)], collapse=".")
    )
    matched <- templ %in% static
    if (any(matched)) {
      index <- as.numeric(names(splithosts)[matched])
      if (split.after>1) {
        subdomain[index] <- sapply(splithosts[matched], function(x) paste(x[1:(split.after-1)], collapse="."))
      }
      domain[index] <- sapply(splithosts[matched], function(x) unlist(x[split.after]))
      tld[index] <- sapply(splithosts[matched], function(x) paste(x[(split.after+1):length(x)], collapse="."))
    }
    # now the wildcard
    matched2 <- templ %in% wildcard
    if (any(matched2) && split.after > 1) {
      safter <-  split.after - 1
      index <- as.numeric(names(splithosts)[matched2])
      if (safter>1) {
        subdomain[index] <- sapply(splithosts[matched2], function(x) paste(x[1:(safter-1)], collapse="."))
      }
      domain[index] <- sapply(splithosts[matched2], function(x) x[safter])
      tld[index] <- sapply(splithosts[matched2], function(x) paste(x[(safter+1):length(x)], collapse="."))
    }
    if (any(matched2 | matched)) {
      splithosts <- splithosts[!(matched | matched2)]
      if(length(splithosts)<1) break
    }
  }  
  data.frame(host=host, subdomain=subdomain, domain=domain, tld=tld, stringsAsFactors=F)
}

#' List of Top-Level Domains Names
#' 
#' A dataset containing a single vector of the top-level domain names as retrevied from
#' \url{https://publicsuffix.org/list/effective_tld_names.dat} on August 2, 2014.
#' 
#'  This can manually refreshed upon library load with the \code{\link{getTLD}} function.
#'  
#'  Note, the non-ASCII characters may create a problem, and needs more testing.
#' 
#' @docType data
#' @keywords datasets
#' @format A vector containing the top level domains
#' @name tldnames
NULL
