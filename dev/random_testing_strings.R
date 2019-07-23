.code_string <- '

iris %>%
    mutate(
      Sepal.Size = Sepal.Length + Sepal.Width
      ) %>%
    group_by(Species) %>%
    summarize(mean(Sepal.Length))

iris %>% summary()

thing <- 1:10

plot(thing)

'

results <- evaluate(str_trim(.code_string))

map(results, class)

demo_code(.code_string)
