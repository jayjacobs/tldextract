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
  content[grep("^//", content, invert=T)]
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
#' @param tlds vector of TLD names (see \code{\link{getTLD}})
#' @import data.table
#' @export
#' @seealso getTLD
#' @examples
#' \dontrun{
#' hosts <- tldextract(c("www.google.com", "testing.co.uk"), tlds=getTLD())
#' }
tldextract <- function(host, tlds=NULL) {
  if (missing(tlds)) {
#     if(exists("tldnames") && is.data.frame(tldnames)) {
#       tlds <- tldnames
#     } else {
      data("tldnames", envir = environment())
#    }
  }
  # http://stackoverflow.com/questions/11486369/growing-a-data-frame-in-a-memory-efficient-manner
  # set up the data.table
  core <- data.table(host=host, tld=NA_character_, domain=NA_character_, subdomain=NA_character_)
  for(i in seq_along(host)) {
    # extract raw elements of host name
    raw <- unlist(strsplit(host[i], "[.]"))
    # may not get subdomain
    subdomain <- domain <- suffix <- NA_character_
    # loop from 2 to the # of elements in host name
    for(tld in seq(2, length(raw))) {
      this.tld <- paste(raw[tld:length(raw)], collapse=".")
      if(this.tld %in% tlds) { # if it is a TLD
        if (tld > 2) { # then get the subdomain if exists
          subdomain <- paste(head(raw, tld-2), collapse=".")
        } # and pull domain and TLD
        domain <- paste(raw[tld-1], collapse=".")
        suffix <- this.tld
        break
      }
    }
    set(core, i, 2L, subdomain) 
    set(core, i, 3L, domain)
    set(core, i, 4L, suffix)
  }
  as.data.frame(core)
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