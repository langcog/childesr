# Accessing \`childes-db\`

## Overview

The `childesr` package allows you to access data in the childes-db from
R. This removes the need to write complex SQL queries in order to get
the information you want from the database. This vignette shows some
examples of how to use the data loading functions and what the resulting
data look like.

There are several different `get_` functions that you can use to extract
different types of data from the childes-db:

- [`get_transcripts()`](https://langcog.github.io/childesr/reference/get_transcripts.md)
- [`get_participants()`](https://langcog.github.io/childesr/reference/get_participants.md)
- [`get_tokens()`](https://langcog.github.io/childesr/reference/get_tokens.md)
- [`get_types()`](https://langcog.github.io/childesr/reference/get_types.md)
- [`get_utterances()`](https://langcog.github.io/childesr/reference/get_utterances.md)
- [`get_speaker_statistics()`](https://langcog.github.io/childesr/reference/get_speaker_statistics.md)
- [`get_sql_query()`](https://langcog.github.io/childesr/reference/get_sql_query.md)

**Technical note 1**: You do not have to explicitly establish a
connection to the childes-db since the `childesr` functions will manage
these connections. But if you would like to establish your own
connection, you can do so with
[`connect_to_childes()`](https://langcog.github.io/childesr/reference/connect_to_childes.md)
and pass it as an argument to any of the `get_` functions. If you do so,
make sure to disconnect the connections you make by using
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html),
[`childesr::clear_connections()`](https://langcog.github.io/childesr/reference/clear_connections.md),
or restarting your R session.

**Technical note 2**: We have tried to optimize the time it takes to get
data from the database. But if you try to query and get all of the
tokens, it will take a long time.

``` r
# load the library
library(childesr)
library(dplyr)
```

## Get transcripts

The `get_transcripts` function returns high-level information about the
transcripts that are available in the database. You can filter your
query to get the transcripts for a specific collection, corpus, or
child.

For example, you can run `get_transcripts` without any arguments to
return all of the transcripts in the database.

``` r
d_transcripts <- get_transcripts()
head(d_transcripts)
```

    ## # A tibble: 6 × 13
    ##   transcript_id corpus_name      language date       filename  target_child_name
    ##           <int> <chr>            <chr>    <chr>      <chr>     <chr>            
    ## 1             1 English-WolfHemp eng      NA         Frogs/En… Margaret         
    ## 2             2 English-WolfHemp eng      1989-06-13 Frogs/En… Zachary          
    ## 3             3 English-WolfHemp eng      1989-06-20 Frogs/En… Andrew           
    ## 4             4 English-WolfHemp eng      1989-07-13 Frogs/En… Christina        
    ## 5             5 English-WolfHemp eng      1989-06-07 Frogs/En… Christopher      
    ## 6             6 English-WolfHemp eng      1989-07-25 Frogs/En… Alexandra        
    ## # ℹ 7 more variables: target_child_age <dbl>, target_child_sex <chr>,
    ## #   collection_name <chr>, pid <chr>, collection_id <int>, corpus_id <int>,
    ## #   target_child_id <int>

If you only want information about a specific collection, such as the
English-American transcripts, then you can specify this in the
collection argument.

``` r
d_eng_na <- get_transcripts(collection = "Eng-NA")
head(d_eng_na)
```

    ## # A tibble: 6 × 13
    ##   transcript_id corpus_name language date  filename            target_child_name
    ##           <int> <chr>       <chr>    <chr> <chr>               <chr>            
    ## 1          3629 Garvey      eng      NA    Eng-NA/Garvey/amya… NA               
    ## 2          3630 Garvey      eng      NA    Eng-NA/Garvey/amyw… NA               
    ## 3          3631 Garvey      eng      NA    Eng-NA/Garvey/arig… NA               
    ## 4          3632 Garvey      eng      NA    Eng-NA/Garvey/arik… NA               
    ## 5          3633 Garvey      eng      NA    Eng-NA/Garvey/bend… NA               
    ## 6          3634 Garvey      eng      NA    Eng-NA/Garvey/bevf… NA               
    ## # ℹ 7 more variables: target_child_age <dbl>, target_child_sex <chr>,
    ## #   collection_name <chr>, pid <chr>, collection_id <int>, corpus_id <int>,
    ## #   target_child_id <int>

If you know the corpus that you want to analyze, then you can specify
this in the corpus argument. The following function call will return
information about all of the transcripts in the Brown corpus.

``` r
# returns all transcripts in the brown corpus
d_brown_transcripts <- get_transcripts(corpus = "Brown")
# print the number of rows
nrow(d_brown_transcripts)
```

    ## [1] 214

If you want more than one corpus, then you can pass a multiple corpus
names. You can also pass more than one name to the collections and child
arguments.

``` r
d_many_corpora <- get_transcripts(corpus = c("Brown", "Clark"))
# print the number of rows
nrow(d_many_corpora)
```

    ## [1] 261

If you want transcript information about a specific child from a corpus,
then you pass their name to the child argument. *Note* that the
following function call will not return any of the transcripts from the
Brown corpus because the child Shem is not present in that corpus.

``` r
d_shem <- get_transcripts(corpus = c("Brown", "Clark"),
                          target_child = "Shem")
# print the number of rows
nrow(d_shem)
```

    ## [1] 47

## Get participants

The `get_participants` function returns background information about the
speakers (both the children and the adults) in the database. This
includes information about:

- the speaker’s role in the conversation
- language
- sex
- SES
- youngest age of transcript
- oldest age of transcript

Again, if you run the function with no arguments, then you get all the
background information for all speakers in the database.

``` r
d_participants <- get_participants()
head(d_participants)
```

    ## # A tibble: 6 × 18
    ##      id code  name  role  corpus_name min_age max_age language group sex   ses  
    ##   <int> <chr> <chr> <chr> <chr>         <dbl>   <dbl> <chr>    <chr> <chr> <chr>
    ## 1     1 CHI   Marg… Targ… English-Wo…    80.0    105. eng      NA    fema… MC   
    ## 2     2 CHI   Chri… Targ… English-Wo…    77.0    105. eng      NA    fema… MC   
    ## 3     3 INV   Nina  Inve… English-Wo…    NA       NA  eng      NA    NA    NA   
    ## 4     4 CHI   Andr… Targ… English-Wo…    77.0    104. eng      NA    male  MC   
    ## 5     5 CHI   Chri… Targ… English-Wo…    79.0    106. eng      NA    male  WC   
    ## 6     6 CHI   Alex… Targ… English-Wo…    79.0    103. eng      NA    fema… WC   
    ## # ℹ 7 more variables: education <chr>, custom <chr>, collection_name <chr>,
    ## #   collection_id <int>, corpus_id <int>, target_child_id <int>,
    ## #   target_child_name <chr>

The participants function introduces three new arguments: role, age, and
sex. The role argument allows you to get information about a specific
kind of speaker, such as the “target_child.”

``` r
d_target_child <- get_participants(role = "target_child")
head(d_target_child)
```

    ## # A tibble: 6 × 18
    ##      id code  name  role  corpus_name min_age max_age language group sex   ses  
    ##   <int> <chr> <chr> <chr> <chr>         <dbl>   <dbl> <chr>    <chr> <chr> <chr>
    ## 1     1 CHI   Marg… Targ… English-Wo…    80.0    105. eng      NA    fema… MC   
    ## 2     2 CHI   Chri… Targ… English-Wo…    77.0    105. eng      NA    fema… MC   
    ## 3     4 CHI   Andr… Targ… English-Wo…    77.0    104. eng      NA    male  MC   
    ## 4     5 CHI   Chri… Targ… English-Wo…    79.0    106. eng      NA    male  WC   
    ## 5     6 CHI   Alex… Targ… English-Wo…    79.0    103. eng      NA    fema… WC   
    ## 6     7 CHI   Mich… Targ… English-Wo…    79.0    105. eng      NA    male  WC   
    ## # ℹ 7 more variables: education <chr>, custom <chr>, collection_name <chr>,
    ## #   collection_id <int>, corpus_id <int>, target_child_id <int>,
    ## #   target_child_name <chr>

The age argument takes a number indicating the age(s) of children (in
months) that you want to analyze. you can use this argument in two ways

1.  Pass a single number to information about all participants who have
    a transcript at that age.
2.  Pass a range of ages to get information about all participants who
    have transcript within a certain age range.

For example, you can get the participant information for all of the
children who had transcripts between the ages of 24 and 36 months.

``` r
d_age_range <- get_participants(age = c(24, 36))
head(d_age_range)
```

    ## # A tibble: 6 × 18
    ##      id code  name  role  corpus_name min_age max_age language group sex   ses  
    ##   <int> <chr> <chr> <chr> <chr>         <dbl>   <dbl> <chr>    <chr> <chr> <chr>
    ## 1  1598 CHI   NA    Targ… Chinese-Ta…    32.2    60.6 zho      NA    male  NA   
    ## 2  1669 CHI   Ian   Targ… Garvey         34.0    34.0 eng      NA    male  NA   
    ## 3  1679 CHI   Nan   Targ… Garvey         34.0    34.0 eng      NA    fema… NA   
    ## 4  1688 NAN   Nan   Targ… Garvey         34.0    34.0 eng      NA    fema… NA   
    ## 5  1701 CHI   Sam   Targ… Garvey         35.0    35.0 eng      NA    male  NA   
    ## 6  1702 CHI   NA    Targ… Valian         21.7    32.8 eng      TD    fema… NA   
    ## # ℹ 7 more variables: education <chr>, custom <chr>, collection_name <chr>,
    ## #   collection_id <int>, corpus_id <int>, target_child_id <int>,
    ## #   target_child_name <chr>

## Get tokens

The `get_tokens` function returns a table with a row for each token
based on a set of filtering criteria. The token argument allows you to
pass a vector of one or more tokens that you want to analyze.

For example, if you wanted to get all of the production data for a
specific token(s), then you could run the following call to get all
instances of “dog” and “ball” for Adam in the Brown corpus.

``` r
d_adam_prod <- get_tokens(corpus = "Brown",
                          role = "target_child",
                          target_child = "Adam",
                          token = c("dog", "ball"))

# view the structure of the data
str(d_adam_prod)
```

    ## tibble [265 × 29] (S3: tbl_df/tbl/data.frame)
    ##  $ id               : int [1:265] 7557235 7557298 7557590 7557593 7557787 7557840 7558259 7558391 7558393 7559092 ...
    ##  $ gloss            : chr [1:265] "dog" "ball" "ball" "ball" ...
    ##  $ language         : chr [1:265] "eng" "eng" "eng" "eng" ...
    ##  $ token_order      : int [1:265] 3 2 2 3 1 3 2 1 1 1 ...
    ##  $ replacement      : chr [1:265] "" "" "" "" ...
    ##  $ prefix           : chr [1:265] "" "" "" "" ...
    ##  $ part_of_speech   : chr [1:265] "n" "n" "n" "n" ...
    ##  $ stem             : chr [1:265] "dog" "ball" "ball" "ball" ...
    ##  $ actual_phonology : chr [1:265] "" "" "" "" ...
    ##  $ model_phonology  : chr [1:265] "" "" "" "" ...
    ##  $ suffix           : chr [1:265] "" "" "" "" ...
    ##  $ num_morphemes    : int [1:265] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ english          : chr [1:265] "" "" "" "" ...
    ##  $ clitic           : chr [1:265] "" "" "" "" ...
    ##  $ utterance_type   : chr [1:265] "declarative" "declarative" "declarative" "declarative" ...
    ##  $ corpus_name      : chr [1:265] "Brown" "Brown" "Brown" "Brown" ...
    ##  $ speaker_code     : chr [1:265] "CHI" "CHI" "CHI" "CHI" ...
    ##  $ speaker_name     : chr [1:265] "Adam" "Adam" "Adam" "Adam" ...
    ##  $ speaker_role     : chr [1:265] "Target_Child" "Target_Child" "Target_Child" "Target_Child" ...
    ##  $ target_child_name: chr [1:265] "Adam" "Adam" "Adam" "Adam" ...
    ##  $ target_child_age : num [1:265] 27.1 27.1 27.1 27.1 27.1 ...
    ##  $ target_child_sex : chr [1:265] "male" "male" "male" "male" ...
    ##  $ collection_name  : chr [1:265] "Eng-NA" "Eng-NA" "Eng-NA" "Eng-NA" ...
    ##  $ collection_id    : int [1:265] 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ corpus_id        : int [1:265] 60 60 60 60 60 60 60 60 60 60 ...
    ##  $ speaker_id       : int [1:265] 3327 3327 3327 3327 3327 3327 3327 3327 3327 3327 ...
    ##  $ target_child_id  : int [1:265] 3327 3327 3327 3327 3327 3327 3327 3327 3327 3327 ...
    ##  $ transcript_id    : int [1:265] 7263 7263 7263 7263 7263 7263 7265 7265 7265 7264 ...
    ##  $ utterance_id     : int [1:265] 1759848 1759936 1760126 1760127 1760262 1760293 1763420 1763944 1763967 1764045 ...

``` r
# print the unique tokens
unique(d_adam_prod$gloss)
```

    ## [1] "dog"  "ball"

## Get types

The
[`get_types()`](https://langcog.github.io/childesr/reference/get_types.md)
function works like the
[`get_tokens()`](https://langcog.github.io/childesr/reference/get_tokens.md)
function, returning a table with a row for each type based on set of
filtering criteria. The type argument allows you to pass a vector of one
or more types that you want to analyze. The main difference is that you
now have a single row for each type (i.e., a concept) and a variable
`count` that tracks the number of times that type appeared in a
particular transcript.

For example, if you wanted to get all of the production data for a
specific type(s), then you could run the following call to get counts of
“dog” and “ball” for all of Adam’s transcripts in the Brown corpus.

``` r
d_adam_types <- get_types(corpus = "Brown",
                          target_child = "Adam",
                          role = "target_child",
                          type = c("dog", "ball"))

# print the number of times ball appears in the first transcript
c(d_adam_types$gloss[1], d_adam_types$count[1])
```

    ## [1] "ball" "26"

## Get utterances

The `get_utterances` function returns a table with a row for each
utterance based on user-defined filtering criteria. For example, the
following function will get you all of the utterances in the Brown
Corpus for the child Adam.

``` r
d_adam_utts <- get_utterances(corpus = "Brown",
                              target_child = "Adam")

# view the structure of the data
str(d_adam_utts)
```

    ## tibble [73,431 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ id               : int [1:73431] 1759250 1759256 1759261 1759264 1759269 1759274 1759279 1759284 1759289 1759294 ...
    ##  $ gloss            : chr [1:73431] "play checkers" "big drum" "big drum" "big drum" ...
    ##  $ stem             : chr [1:73431] "play checker" "big drum" "big drum" "big drum" ...
    ##  $ actual_phonology : chr [1:73431] "" "" "" "" ...
    ##  $ model_phonology  : chr [1:73431] "" "" "" "" ...
    ##  $ type             : chr [1:73431] "declarative" "declarative" "question" "declarative" ...
    ##  $ language         : chr [1:73431] "eng" "eng" "eng" "eng" ...
    ##  $ num_morphemes    : int [1:73431] 3 2 2 2 2 2 1 1 2 4 ...
    ##  $ num_tokens       : int [1:73431] 2 2 2 2 2 2 1 1 2 3 ...
    ##  $ utterance_order  : int [1:73431] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ corpus_name      : chr [1:73431] "Brown" "Brown" "Brown" "Brown" ...
    ##  $ part_of_speech   : chr [1:73431] "n n" "adj n" "adj n" "adj n" ...
    ##  $ speaker_code     : chr [1:73431] "CHI" "CHI" "MOT" "CHI" ...
    ##  $ speaker_name     : chr [1:73431] "Adam" "Adam" NA "Adam" ...
    ##  $ speaker_role     : chr [1:73431] "Target_Child" "Target_Child" "Mother" "Target_Child" ...
    ##  $ target_child_name: chr [1:73431] "Adam" "Adam" "Adam" "Adam" ...
    ##  $ target_child_age : num [1:73431] 27.1 27.1 27.1 27.1 27.1 ...
    ##  $ target_child_sex : chr [1:73431] "male" "male" "male" "male" ...
    ##  $ media_start      : num [1:73431] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ media_end        : num [1:73431] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ media_unit       : chr [1:73431] NA NA NA NA ...
    ##  $ collection_name  : chr [1:73431] "Eng-NA" "Eng-NA" "Eng-NA" "Eng-NA" ...
    ##  $ collection_id    : int [1:73431] 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ corpus_id        : int [1:73431] 60 60 60 60 60 60 60 60 60 60 ...
    ##  $ speaker_id       : int [1:73431] 3327 3327 3329 3327 3327 3327 3327 3329 3327 3329 ...
    ##  $ target_child_id  : int [1:73431] 3327 3327 3327 3327 3327 3327 3327 3327 3327 3327 ...
    ##  $ transcript_id    : int [1:73431] 7263 7263 7263 7263 7263 7263 7263 7263 7263 7263 ...

``` r
# print the first five utterances
d_adam_utts$gloss[1:5]
```

    ## [1] "play checkers" "big drum"      "big drum"      "big drum"     
    ## [5] "big drum"

## Get speaker statistics

The
[`get_speaker_statistics()`](https://langcog.github.io/childesr/reference/get_speaker_statistics.md)
function returns a table with a row for each transcript and columns that
contain a set of summary statistics for that transcript. The summary
statistics include:

- number of utterances (`num_utterances`)
- number of types (`num_types`)
- number of tokens (`num_tokens`)
- number of morphemes (`num_morphemes`)
- mean length of utterances in words (`mlu_w`)
- mean length of utterances in morphemes (`mlu_m`)

For example, if we wanted to get the summary statistics for Adam’s
production data, we could run the following call.

``` r
d_adam_stats <- get_speaker_statistics(corpus = "Brown",
                                       target_child = "Adam",
                                       role = "target_child")

# get the average mlu across all Adam's transcripts
mean(d_adam_stats$mlu_w)
```

    ## [1] 3.559118

### Get SQL Query

The
[`get_sql_query()`](https://langcog.github.io/childesr/reference/get_sql_query.md)
function returns a table from a SQL query run on the specified database.
For example, if you wanted to see the top 10 corpora in the `Eng-NA`
collection with the highest count of token for “dog”, you could run the
following call.

``` r
d_na_dog <- get_sql_query("SELECT corpus_name, COUNT(id) AS count FROM token WHERE collection_name = 'Eng-NA' AND gloss = 'dog' GROUP BY corpus_name")

dplyr::arrange(d_na_dog, desc(count))
```

    ## # A tibble: 55 × 2
    ##    corpus_name  count
    ##    <chr>        <dbl>
    ##  1 HSLLD         1261
    ##  2 Providence     862
    ##  3 Brown          546
    ##  4 Gelman         537
    ##  5 Weist          437
    ##  6 NewmanRatner   433
    ##  7 Suppes         421
    ##  8 Hall           337
    ##  9 Brent          277
    ## 10 Davis          241
    ## # ℹ 45 more rows
