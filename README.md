This package is no longer active.  All of the functionality has been integrated into the `urltools` package:
https://cran.r-project.org/web/packages/urltools/index.html





After working with <https://github.com/john-kurkowski/tldextract> in
python, I wanted the same functionality within R. The list of top level
domains can be automatically loaded from
<https://publicsuffix.org/list/effective_tld_names.dat>. A cached
version of the data is stored in the package.

### Installation

To install this package, use the devtools package:

    devtools::install_github("jayjacobs/tldextract")

### Usage

    library(tldextract)

    ## Loading required package: data.table
    ## Loading required package: httr

    # use the cached lookup data, simple call
    tldextract("www.google.com")

    ##             host subdomain domain tld
    ## 1 www.google.com       www google com

    # it can take multiple domains at the same time
    tldextract(c("www.google.com", "www.google.com.ar", "googlemaps.ca", "tbn0.google.cn"))

    ##                host subdomain     domain    tld
    ## 1    www.google.com       www     google    com
    ## 2 www.google.com.ar       www     google com.ar
    ## 3     googlemaps.ca      <NA> googlemaps     ca
    ## 4    tbn0.google.cn      tbn0     google     cn

The specification for the top-level domains is cached in the package and
is viewable.

    # view and update the TLD domains list in the tldnames data
    data(tldnames)
    head(tldnames)

    ## [1] "ac"     "com.ac" "edu.ac" "gov.ac" "net.ac" "mil.ac"

If the cached version is out of data and the package isn't updated, the
data can be manually loaded, and then passed into the function.

    # get most recent TLD listings
    tld <- getTLD() # optionally pass in a different URL than the default
    manyhosts <- c("pages.parts.marionautomotive.com", "www.embroiderypassion.com", 
                   "fsbusiness.co.uk", "www.vmm.adv.br", "ttfc.cn", "carole.co.il",
                   "visiontravail.qc.ca", "mail.space-hoppers.co.uk", "chilton.k12.pa.us")
    tldextract(manyhosts, tldnames=tld)

    ##                               host   subdomain            domain       tld
    ## 1 pages.parts.marionautomotive.com pages.parts  marionautomotive       com
    ## 2        www.embroiderypassion.com         www embroiderypassion       com
    ## 3                 fsbusiness.co.uk        <NA>        fsbusiness     co.uk
    ## 4                   www.vmm.adv.br         www               vmm    adv.br
    ## 5                          ttfc.cn        <NA>              ttfc        cn
    ## 6                     carole.co.il        <NA>            carole     co.il
    ## 7              visiontravail.qc.ca        <NA>     visiontravail     qc.ca
    ## 8         mail.space-hoppers.co.uk        mail     space-hoppers     co.uk
    ## 9                chilton.k12.pa.us        <NA>           chilton k12.pa.us
