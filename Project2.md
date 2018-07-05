Data introduction
-----------------

As required, this task was an open one, so the students had to choose a
specific topic on their own. Our Group did choose a dataset we found on
<https://labrosa.ee.columbia.edu/millionsong/pages/getting-dataset#subset>.
This subset Contains 10k Music files and is around 2GB big. The actual
dataset is about 300GB big and has arround 1 MIllion entries, in this
case songs. Besids the Analysis, the dataset contains also some
Metadata, like Author, produced year etc. and finally music data in HDF5
format. The actual Provider of this data set is THE ECHO NEST
(<http://the.echonest.com>). As provided by the information about the
dataset, it is a result of an collaboration between THE ECHO NEST and
LabROSA (<https://labrosa.ee.columbia.edu>).

The H5 data explained:
<https://labrosa.ee.columbia.edu/millionsong/pages/example-track-description>

-   Nehme eine Menge an Songs die uns gut gefallen schau sie dir an
-   nehme eine weiter menge vergleiche diese miteinander und höre sie
    dir an und gib dein final ergebnis

Handle the downloaded data
--------------------------

After downloading and unzipping the data, one can see two different
folders, one 'data' containing several other folders 'A' and 'B' which
contain songfiles in HDF5 (Hirarchical Data Format 5) format. This
format is a general used format in science for big datasets. This files
contain some analysis, some metadata and some more information that is
stored on MusicBrainz (<https://musicbrainz.org>). about Songs that is
still used in scientific applications and using and more saving big
datasets. For Our purposes we only will use the dataset with the
metadata all HDF5 file we will not consider anymore.

Preprocess the Data
-------------------

first one has to remove teh Seperators <SEP> and replace those with a
common seperator like ';' because R is used to a one byte seperator and
therefor it is not possible to do so in R itself.

    location <- read.csv2('data/subset_artist_location.txt',sep = ';', header = FALSE, col.names = c('artistId', 'lat','lon',  'trackID', 'artistName'))

    artists <- read.csv2('data/subset_unique_artists.txt',sep = ';', header = FALSE, col.names = c('artistId', 'V2', 'trackID', 'artistName'))

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec =
    ## dec, : EOF within quoted string

    tags <- read.csv2('data/subset_unique_mbtags.txt',sep = ';', header = FALSE, col.names = c('tags'))

    uni_terms <- read.table('data/subset_unique_terms.txt',sep = ';', header = FALSE, col.names = c('terms Unique' ))

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec =
    ## dec, : EOF within quoted string

    tracks <- read.csv2('data/subset_unique_tracks.txt',sep = ';', header = FALSE, col.names = c('trackID','V2', 'artistName','songName'))

    tracksPerYear <- read.csv2('data/subset_tracks_per_year.txt',sep = ';', header = FALSE,  col.names = c('Year', 'trackID', 'artistName','songName'))

    ## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec =
    ## dec, : EOF within quoted string

    # Install
    #install.packages("tm")  # for text mining
    #install.packages("SnowballC") # for text stemming
    #install.packages("wordcloud") # word-cloud generator 
    #install.packages("RColorBrewer") # color palettes

    # Load
    library("tm")

    ## Loading required package: NLP

    library("SnowballC")
    library("wordcloud")

    ## Loading required package: RColorBrewer

    library("RColorBrewer")


    docs <- Corpus(VectorSource(as.String(artists$artistName)))

    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))

    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)

    ##                word freq
    ## the             the  350
    ## and             and   46
    ## john           john   41
    ## orchestra orchestra   40
    ## band           band   35
    ## los             los   31
    ## david         david   30
    ## featuring featuring   26
    ## feat           feat   25
    ## joe             joe   21

    # important for better plot 
    logFreq <- ceiling(log(d$freq)*4)

    wordcloud(words = d$word, freq = logFreq, min.freq = 1, scale= c(2,0.35),
              max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

![](Project2_files/figure-markdown_strict/pressure-1.png)

    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "the")

    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)

    wordcloud(words = d$word, freq = d$freq, min.freq = 1, scale= c(2,0.35),
              max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

![](Project2_files/figure-markdown_strict/pressure-2.png)

    docs1 <- Corpus(VectorSource(as.String(tracks$songName)))

    # Convert the text to lower case
    docs1 <- tm_map(docs1, content_transformer(tolower))

    dtm <- TermDocumentMatrix(docs1)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)

    ##            word freq
    ## the         the 1703
    ## version version  773
    ## you         you  540
    ## album     album  467
    ## love       love  332
    ## and         and  243
    ## live       live  216
    ## for         for  185
    ## all         all  144
    ## your       your  143

    # important for better plot 
    logFreq <- ceiling(log(d$freq)*4)

    wordcloud(words = d$word, freq = logFreq, min.freq = 1, scale= c(1,0.35),
              max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

![](Project2_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    # install.packages('rworldmap')
    library(rworldmap)

    ## Loading required package: sp

    ## ### Welcome to rworldmap ###

    ## For a short introduction type :   vignette('rworldmap')

    newmap <- getMap(resolution = "low")


    lon <- as.double(as.character(location$lon))
    lat <- as.double(as.character(location$lat))

    ## Warning: NAs introduced by coercion

    lon <- lon[!is.na(lon)]
    lat <- lat[!is.na(lat)]

    #plot.new()
    #plot(newmap, xlim = c(-180, 180), ylim = c(-100, 100), asp = 1)


    #summary(newmap)
    plot(newmap, xlim = c(-130, 130), ylim = c(-90, 85), asp = 1)
    par(new = TRUE)
    plot(lon, lat, col = "red", cex = .3)

![](Project2_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

    #source("http://bioconductor.org/biocLite.R")
    #biocLite("rhdf5")
    #library(rhdf5)
    #path <- '/Users/Kostja/Desktop/Master/Sem 2 (18 SoSe)/Data #Visualization/Tasks/DV_Project_2/data/A/TRAAAAW128F429D538.h5'
    #my.data <- h5ls("/Users/Kostja/Desktop/Master/Sem 2 (18 SoSe)/Data #Visualization/Tasks/DV_Project_2/data/A/TRAAAAW128F429D538.h5")
    #mydata <- h5read(path, "/metadata/artist_terms")
    #mydata1 <- h5read(path, "/analysis/songs")
    #mydata2 <- h5read(path, "/metadata/similar_artists")
    #mydata3 <- h5read(path, "/musicbrainz/artist_mbtags")
    #mydata4 <- h5read(path, "/metadata")
    #mydata4 <- h5read(path, "/analysis/segments_loudness_start")
