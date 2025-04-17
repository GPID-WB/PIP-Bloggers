# Recap-----------------------------------------------------
#-----------------------------------------------------------

# Named list, except:
#
#   1) Every name must be unique.
#
#   2) The names in an environment are not ordered.
#
#   3) An environment has a parent.
#
#   4) Environments are not copied when modified.

# Lexical scoping:
#    process of looking for a value associated with a name
#    determines where to look for the value

# example 1
library(rlang)
current_env()
x <- 10
g01 <- function() {
  x <- 20
  x
}
g01()
x

# example 2
x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()


# example 3
g03 <- function() {
  y <- 1
  c(x, y)
}
g03()
x
y


# example 4
g04 <- function() {

  y <- 2
  i <- function() {
    z <- 3
    c("x" = x,
      "y" = y,
      "z" = z)
  }

  i()

}
g04()
x
y
z

# example 5
g05 <- function() {

  x <- 11

  g04b <- function() {
    y <- 2
    i <- function() {
      z <- 3
      c("x" = x,
        "y" = y,
        "z" = z)
    }
    i()
  }

  list(g04(), g04b())

}
x <- 100
g05()



# example 6
g06 <- function() {

  # define internal function
  g06a <- function() {
    current_env()
  }

  # print envs
  print(current_env())
  print(environment(g06a))
  print(environment(g05))
  g06a()
}
g06()
environment(g06)

# Basics
#-----------
library(rlang)

e1 <- env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3)
e1$d <- e1
env_print(e1)
env_names(e1)

# subset: which ones work?
e1[["a"]]
e1["a"]
e1[1]
e1[[1]]
e1$a

# parents
e2a <- env(d = 4,
           e = 5)
e2b <- env(e2a,
           a = 1,
           b = 2,
           c = 3)
env_parent(e2a)
env_parent(e2b)
env_parents(e2b)
env_parents(e2b, empty_env())

env_get(e2b, "a")
env_get(e2b, "d")
env_get(e2b, "d", inherit = T)
env_get(e2b, "d", default = NA)

# create new object binding
xname <- "newname"
env_poke(e2b,
         xname,
         "check")
env_names(e2b)
env_bind(e2b,
         new2 = 101,
         new3 = 102)
e2b$new2

assign(x     = "using_assign",
       value = "random country Colombia",
       envir = e2b)

e2b$using_assign

e2b$another_way <- "random country South Africa"
env_get(e2b, "another_way")

# Does binding exist?
exists("using_assign", e2b)
e2b$using_assign
env_has(e2b, "using_assign")

# 'Hidden' environment arguments in functions
get("new1", envir = e2b) # does this look familiar?

# Give me the value of the object named "x" in the current environment (or the one I specify with envir = ...)
var <- "c"
e2b$var <- "c"

data.frame(a = 1:10,
           b = 1:10,
           c = 1:10) |>
  fsubset(get(var) > 5) # looks for 'c' in current environment

fsubset(data.frame(a = 1:10,
                   d = 1:10,
                   c = 1:10),
        get(var, envir = e2b) > 5)

fsubset(data.frame(a = 1:10,
                   d = 1:10,
                   c = 1:10),
        e2b$var > 5)

data.frame(a = 1:10,
           b = 1:10,
           c = 1:10) |>
  fsubset(var > 5)



data.frame(a = 1:10,
           b = 1:10,
           c = 1:10) |>
  fsubset("c" > 5)



# Example: Counter---------------
#--------------------------------

# memoryless
g <- function() {

  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g()
g()

# With memory
new_counter <- function() {
  i <- 0

  print(current_env())

  # create function
  out_function <- function() {
    i <<- i + 1
    i
  }

  # return function
  out_function
}

# alternative is: e <- current_env()
#                 e$i <- i + 1

counter <- new_counter()
counter()
counter()
i

env <- environment(counter)
env_get(env, "i")
counter()
env$i <- 100
counter()
env_get(env, "i")

counter2 <- new_counter()

counter2()
counter()

# Example: Memoization----------------------
#-------------------------------------------

df <- data.frame(
  id    = 1:1e6,
  group = sample(letters[1:5],
                 1e6,
                 replace = TRUE),
  value = rnorm(1e6))
df |> head()


expensive_summary <- function(threshold) {
  df |>
    fsubset(value > threshold) |>
    fgroup_by(group) |>
    fsummarise(sd = fsd(value))
}


# Computations occur twice:
expensive_summary(1.5)
expensive_summary(1.5)


memoize_summary <- function() {

  # create cache environment
  cache <- new.env(parent = emptyenv())

  # define function
  out_function <- function(threshold) {

    key <- as.character(threshold)

    if (exists(key, envir = cache)) {
      # no computation
      cli::cli_alert_info("Returning cached result for {.field threshold} = {threshold}")
      out <- cache[[key]]
    } else {
      # computation
      cache[[key]] <- expensive_summary(threshold)

      cli::cli_alert_success("Computed and cached result for {.field threshold} = {threshold}")

      out <- cache[[key]]
    }

    out

  }

  # return function
  out_function

}

# Create a memoized version (function factory)
cached_summary <- memoize_summary()
cached_summary2 <- memoize_summary()

# First time: will compute
out1 <- cached_summary(1.5)

# Second time with same threshold: fast, uses cache
out2 <- cached_summary(1.5)

# New threshold triggers a new computation
out3 <- cached_summary(2.0)


identical(out1, out2)
identical(out1, out3)
identical(expensive_summary(1.5),
          out1)

cached_summary2(1.5)


# objective function
# gradient obj

maxLik












