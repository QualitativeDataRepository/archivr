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

The basic function is `archiv` that takes a list of urls and checks their
availability on the wayback machine. It will return a dataframe with the
original urls followed by the http status (or 000 if no url exists), their
availability (TRUE or FALSE), the wayback machine url, and a timestamp.

```
arc_df <- archiv(list("www.example.com", "NOTAURL", "www.github.com"))
arc_df$status # [1] 200 000 200 / Levels: 000 200
arc_df$wayback_url # [1] http://web.archive.org/web/20181214234252/http://Www.example.com
              # [2] url not found                                                   
              # [3] http://web.archive.org/web/20181215081640/https://github.com/   
              # 3 Levels: http://web.archive.org/web/20181214234252/http://Www.example.com ...
```
To check a list of urls in perma_cc, use:
```
arc_url_df <- archiv(list("www.example.com", "NOTAURL", "www.github.com"), "perma_cc")
```

or you may use both:
```
arc_url_df <- archiv(list("www.example.com", "NOTAURL", "www.github.com"), "both")
```

Archiv can also check a webpage for archived urls.

```
arc_url_df <- archiv.fromUrl("https://qdr.syr.edu/")
df <- data.frame(arc_url_df$url, arc_url_df$wayback_url)[8,]

#   arc_url_df.url                                    arc_url_df.wayback_url
# 8 http://syr.edu http://web.archive.org/web/20170110050058/http://syr.edu/
```




## TESTING

Archivr has a few unit tests that can be run for contributors. To run, use
`r -f run_tests.R` inside the archivr folder.

### Archivr was developed by Ryan Deschamps @greebie
