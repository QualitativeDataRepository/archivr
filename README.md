# Archivr

Archivr is a project by the [Qualitative Data Repository](https://qdr.syr.edu/)
that automates preservation of urls in Web Archives.


## Installation

The easiest way to install is directly from this github using the `devtools` package:

```
library(devtools)
install_github("QualitativeDataRepository/archivr")
library(archivr)
```

## Usage

The basic function is `archiv` that takes a list of urls and stores them in
the Way Back Machine. It will return a dataframe containing the callback
data for the service.

```
arc_df <- archiv(list("www.example.com", "NOTAURL", "www.github.com"))
arc_df$way_back_url   
#                                                        wayback_url
# 1 http://web.archive.org/web/20190128171132/http://www.example.com
# 2                                                    url not found
# 3    http://web.archive.org/web/20190128171134/https://github.com/ ...
```


Archiv can archive all the urls in a webpage. This feature is subject to restrictions
imposed on accounts

```
arc_url_df <- archiv.fromUrl("https://qdr.syr.edu/")
df <- data.frame(arc_url_df$url, arc_url_df$wayback_url)[8,]

#   arc_url_df.url                                    arc_url_df.wayback_url
# 8 http://syr.edu http://web.archive.org/web/20170110050058/http://syr.edu/
```

Archiv will also archive all the urls in a text file. It has been tested for docx,
pdf and markdown, although other text-related files should also work. Note that
text parsing can be subject to problems, especially if the document has rich features
such as tables or columns.
```
arc_url_df <- archiv.fromText("path_to_file")
```

To allow for pre-processing of URLs before archiving, `archivr` also provides access to the funcitons used to extract URLs from a webpage (`extract_urls_from_webpage("URL")`), from a files (`extract_urls_from_text("filepath")`) (tested for .docx, markdown, and pdf), and from any supported text file in a folder (`extract_urls_from_folder("filepath")`)

### Checking archiving status

You can check whether URLs are archived by the Internet Archive's Wayback machine:
```
arc_url_df <- view_archiv(list("www.example.com", "NOTAURL", "www.github.com"), "wayback")
```

### Using Perma.cc

If you wish to use perma.cc's archive, you will need to set your api key using:

```
set_api_key("YOUR_API_KEY")
```

if you wish to save the urls in a particular perma.cc folder, you will need to set the default
folder id using

```
set_folder_id("FOLDER_ID")
```

If you do not remember the ids of your folders, you can retrieve these in a dataframe
using:
```
get_folder_ids()
```

You can check your current folder using
```
get_folder_id()
```

and then you can archive materials:

```
arc_df <- archiv(list("www.example.com", "NOTAURL", "www.github.com"), "perma_cc")
```

To check if a list of urls are archived in perma_cc's public api, use:
```
arc_url_df <- view_archiv(list("www.example.com", "NOTAURL", "www.github.com"), "perma_cc")
```


**Archivr is a project developed by the Qualitative Data Repository at Syracuse
University, authored by Ryan Deschamps (greebie on github.com) and Agile Humanities.**
