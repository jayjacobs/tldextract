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
  tl2 <- sub('[*]', "[^.]+", paste0(tldnames,"$"))
  core <- data.table(host=host, subdomain=NA_character_, domain=NA_character_, tld=NA_character_)
  for(i in seq_along(host)) {
    thisip <- host[i]
    # loop on all the TLD's comparing to this
    allmatch <- sapply(tl2, regexec, thisip, ignore.case=T)
    # pull out the match point of the first one
    skips <- min(unlist(allmatch[allmatch>0]))
    # that's the TLD
    set(core, i, 4L, substring(thisip, skips)) # suffix
    # now grab everything else and dig for domain and subdomain
    therest <- unlist(strsplit(substring(thisip, 1, skips-2), "[.]"))
    set(core, i, 3L, therest[length(therest)])
    if(length(therest) > 1) {
      set(core, i, 2L, paste(therest[1:length(therest)-1], collapse=".")) 
    }
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

# tests <- c(
#   "tinong.net", "mdotm.co", "softbank.ne.jp", "mundopositivo.com.br",
#   "pianomedia.eu", "elasticad.net", "dowload.vn", "afx.ms", "sspicy.ru",
#   "crosspixel.net", "alarabiya.net", "kijiji.net", "pubx.ch", "webads.it",
#   "cinergroup.com.tr", "upu.int", "r10.io", "baixarfilmesdublados.net",
#   "yjtag.jp", "easytaxi.com.br", "makeameme.org", "oveloker.eu", "ulabs.me",
#   "datingfactory.net", "arcor.de", "akam.net", "comporium.net",
#   "metadrol.pl", "getmyip.org", "getspeedofit2014.co.uk",
#   "caixa2014.com.br", "vivo.com.br", "zol.com.cn", "partiuviagens.com.br",
#   "livefreefuns.net", "zen.co.uk", "bungie.net", "cursos24horas.com.br",
#   "econda-monitor.de", "costco.ca")

