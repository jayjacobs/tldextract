context("TLD Extract")

test_that("checks simple hostname", {
  expect_identical(
    tldextract("www.google.com"),
    data.frame(host="www.google.com", subdomain="www", 
               domain="google", tld="com", stringsAsFactors=F))
  
})
test_that("checks multiple hostname", {
  hosts <- c("www.tinong.net", "mdotm.co", "mx.mail.softbank.ne.jp", 
             "mundopositivo.com.br", "www.pianomedia.eu", 
             "pages.cinergroup.com.tr", "baixarfilmesdublados.net")
  subdomain <- c("www", NA, "mx.mail", NA, "www", "pages", NA)
  domain <- c("tinong", "mdotm", "softbank", "mundopositivo", 
              "pianomedia", "cinergroup", "baixarfilmesdublados")
  tld <- c("net", "co", "ne.jp", "com.br", "eu", "com.tr", "net")
  hostexpect <- data.frame(host=hosts, subdomain=subdomain,
                            domain=domain, tld=tld,
                            stringsAsFactors=F)
  expect_identical(tldextract(hosts), hostexpect)
})