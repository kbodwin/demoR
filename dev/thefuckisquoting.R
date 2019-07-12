

bob <- quote(x <-  mean(iris$Sepal.Length))

bob

bob[1]
bob[2]

bob[[1]]
bob[[2]]
bob[[3]]

bob[[3]][1]
bob[[3]][2]

bob[[3]][[1]]
bob[[3]][[2]]

#Figuring out types:

is.atomic(bob[[1]])
is.call(bob[[1]])
is.name(bob[[1]])
is.pairlist(bob[[1]])

is.atomic(bob[1])
is.call(bob[1])
is.name(bob[1])

is.atomic(bob[[2]])
is.call(bob[[2]])
is.name(bob[[2]])
is.pairlist(bob[[2]])

is.atomic(bob[[3]])
is.call(bob[[3]])
is.name(bob[[3]])
is.pairlist(bob[[3]])


### Try to find all function names....?

bob <- quote(x <-  mean(sqrt(iris$Sepal.Length)))


get_function_names <- function(quote){

  fns <- NULL

  for(i in 1:length(quote)){

    if(is.call(quote[i])){

      fns <- c(fns, quote[[i]])

    }else if(is.pairlist(quote[i])){

      fns <- c(fns, get_function_names(quote[i]))
    }

  }

  return(fns)
}

get_function_names(bob)
