library(tidyverse)

#### Design.... ####

# Using comments is nice because the code runs
iris %>%
  group_by(Species) %>%  # hlt
  summarize(mean = mean(Sepal.Length) )

# Same with curlies?  Nope.

# So what about doing stuff to individual word :(

iris %>%
  group_by(Species) %>%  # hlt("Species")... repetitive
    summarize(mean = mean(Sepal.Length))


# What do I want?

iris %>%
  group_by(Species) %>%  # hlt
  summarize(mean = mean(Sepal.Length))
