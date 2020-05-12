#### Experimental image comparison procedure ###################################

### Load libraries #############################################################

library(tidyverse)
library(imager)


### Load test images ###########################################################

kj_1 <- load.image("data/kj_1.jpg") # Base image
ab_1 <- load.image("data/ab_1.jpg") # This is a match, but different resolution
ab_2 <- load.image("data/ab_2.jpg") # This is not a match, but looks similar


### Split the images into 10 groupings along the x-axis ########################

kj_1_split <- imsplit(kj_1, "x", 10)
ab_1_split <- imsplit(ab_1, "x", 10)
ab_2_split <- imsplit(ab_2, "x", 10)


### Calculate average colours across the rows of each list element #############

kj_1_means <- 
  kj_1_split %>% 
  map(rowMeans) %>% 
  map_dbl(mean)

ab_1_means <- 
  ab_1_split %>% 
  map(rowMeans) %>% 
  map_dbl(mean)

ab_2_means <- 
  ab_2_split %>% 
  map(rowMeans) %>% 
  map_dbl(mean)


### Compute correlations #######################################################

cor(kj_1_means, ab_1_means) # Correct match:    0.99999
cor(kj_1_means, ab_2_means) # Incorrect match: -0.34999


### One-shot function for caclulating correlation between two images ###########

correlate_images <- function(img_1, img_2, chunks = 25) {
  cor({
    img_1 %>%
      imsplit("x", chunks) %>%
      map(rowMeans) %>%
      map_dbl(mean)
  },
  {
    img_2 %>%
      imsplit("x", chunks) %>%
      map(rowMeans) %>%
      map_dbl(mean)
  })
}


### Comparing different split results ##########################################

split_results <- 
  tibble(
    chunks = 5 * 1:20,
    results_true = map_dbl(chunks, ~correlate_images(kj_1, ab_1, .x)),
    results_false = map_dbl(chunks, ~correlate_images(kj_1, ab_2, .x)),
    time = map_dbl(chunks, ~{
      bench::mark(correlate_images(ab_1, kj_1, .x))$median
    })
  )

# True result correlation declines linearly with more chunks
ggplot(split_results, aes(chunks, results_true)) +
  geom_smooth() +
  geom_point()

# False result correlation is erratic then stable after 25 chunks
ggplot(split_results, aes(chunks, results_false)) +
  geom_smooth() +
  geom_point()

# Timing increases linearly
ggplot(split_results, aes(chunks, time)) +
  geom_smooth() +
  geom_point()

# So 25 chunks seems like a good default

