bob <- "hi I'm kelly"

bob2 <- "hi there I am kel"


my_diff <- ses(unlist(str_split(bob, '')), unlist(str_split(bob2, '')))

add <- str_subset(my_diff, "a") %>% str_split("a")

delete <- str_subset(my_diff, "d")

insert <- str_subset(my_diff, "c")


