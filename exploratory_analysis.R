library(tidyverse)

songs <- read_csv("data/spotify_songs.csv")

dim(songs)

songs_clean <- songs |>
  filter(
    !is.na(track_name),
    !is.na(track_artist)
  )

nrow(songs_clean)

songs_clean <- songs_clean |>
  distinct(track_id, .keep_all = TRUE)

nrow(songs_clean)

songs_clean <- songs_clean |>
  mutate(
    track_name_clean = str_to_lower(track_name),
    track_artist_clean = str_to_lower(track_artist)
  ) |>
  arrange(desc(track_popularity)) |>
  distinct(
    track_name_clean,
    track_artist_clean,
    .keep_all = TRUE
  ) |>
  select(-track_name_clean, -track_artist_clean)

nrow(songs_clean)

feature_long <- songs_clean |>
  select(
    valence,
    energy,
    danceability,
    acousticness,
    loudness,
    tempo
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "value"
  )

head(feature_long)


ggplot(feature_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#7FA6B8", color = "white") +
  facet_wrap(~ feature, scales = "free", ncol = 3) +
  labs(
    x = NULL,
    y = "Number of Songs"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave(
  "feature_distributions.png",
  width = 12,
  height = 7,
  dpi = 300
)

cor_data <- songs_clean |>
  select(
    valence,
    energy,
    danceability,
    acousticness,
    loudness,
    tempo
  )

cor_matrix <- cor(
  cor_data,
  use = "complete.obs"
)

round(cor_matrix, 2)

cor_long <- as.data.frame(
  as.table(cor_matrix)
)

head(cor_long)

p_correlation <- ggplot(
  cor_long,
  aes(x = Var1, y = Var2, fill = Freq)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(Freq, 2)),
    size = 4
  ) +
  scale_fill_gradient2(
    low = "#3B6FB6",
    mid = "white",
    high = "#B84A4A",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

p_correlation

ggsave(
  "feature_correlation_heatmap.png",
  plot = p_correlation,
  width = 9,
  height = 8,
  dpi = 300
)

songs_genre <- songs |>
  filter(
    !is.na(track_name),
    !is.na(track_artist)
  ) |>
  distinct(track_id, playlist_genre, .keep_all = TRUE)

p_genre_overlap <- ggplot(
  songs_genre,
  aes(
    x = valence,
    y = energy,
    color = playlist_genre
  )
) +
  geom_point(
    alpha = 0.18,
    size = 1
  ) +
  labs(
    x = "Valence",
    y = "Energy",
    color = "Genre"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank()
  )

p_genre_overlap

ggsave(
  "genre_overlap.png",
  plot = p_genre_overlap,
  width = 10,
  height = 7,
  dpi = 300
)












