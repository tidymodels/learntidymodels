## code to prepare `spotify` dataset

library(tidyverse)

set.seed(1234)
spotify <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv') %>%
    mutate(
        track_album_release_year =
            track_album_release_date %>%
            str_split("-", simplify = TRUE) %>%
            .[,1] %>%
            as.integer(),
        playlist_genre = recode(playlist_genre, `r&b`="rnb"),
        playlist_genre = as.factor(playlist_genre)

    ) %>%
    select(-track_album_release_date) %>%
    slice_sample(prop = 0.2, replace=FALSE)


usethis::use_data(spotify, overwrite = TRUE)
