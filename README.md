# Archivr

Archivr is a project by the [Qualitative Data Repository](https://qdr.syr.edu/)
that verifies the preservation of urls in Web Archives.

Basic usage (for now):

```
git clone 'https://github.com/QualitativeDataRepository/archivr.git'
cd archivr
```
Then launch R and then:

```
source('archivr.R')
archiv(list("www.example.com"))
```

Examples:

The basic function is `archiv` that takes a list of urls and stores them in
perma_cc. It will return a dataframe with the
original urls followed by the GUID, a timestamp, the wayback machine url, and
the url for the perma_cc screenshot.

```
arc_df <- archiv(list("www.example.com", "NOTAURL", "www.github.com"))
arc_df$perma_cc_url # [1] [1] //perma-archives.org/warc/G5EH-JA9M/http://www.google.com
                    # [2] no url                                                    
                    # [3] //perma-archives.org/warc/QD3H-3FHP/http://www.example.org
                    # 3 Levels: //perma-archives.org/warc/G5EH-JA9M/http://www.google.com ...
```
To check if a list of urls are archived in perma_cc's public api, use:
```
arc_url_df <- view_archiv(list("www.example.com", "NOTAURL", "www.github.com"), "perma_cc")
```

or you may check the Wayback machine:
```
arc_url_df <- archiv(list("www.example.com", "NOTAURL", "www.github.com"), "wayback")
```

Archiv can also check a webpage for archived urls.

```
arc_url_df <- view_archiv.fromUrl("https://qdr.syr.edu/")
df <- data.frame(arc_url_df$url, arc_url_df$wayback_url)[8,]

#   arc_url_df.url                                    arc_url_df.wayback_url
# 8 http://syr.edu http://web.archive.org/web/20170110050058/http://syr.edu/
```




## TESTING

Archivr has a few unit tests that can be run for contributors. To run, use
`r -f run_tests.R` inside the archivr folder.

### Archivr was developed by Ryan Deschamps @greebie with support from the
### Qualitative Data Repository at Syracuse University.
