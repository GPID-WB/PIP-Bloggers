## Retrieve Variables

env <- rlang::env(x = 100)
get("x", envir = env)

x <- 2
get("x")

## ----------------------------------------------------------------------
# this is error
var <- quote(x + 1)
get(var, envir = env) # does not evaluate expressions


## Get is not enough
log_get <- function(var_name, env = parent.frame()) {
  value <- get(var_name, envir = env)
  cat("Found", var_name, "with value:", value, "\n")
}


## ----------------------------------------------------------------------
x <- 42
log_get("x")

## ----------------------------------------------------------------------
log_get("x + 1") # error


## Why eval() is much better than get()

env <- rlang::env(x = 100)
expr <- quote(x + 1)
expr <- quote(x)
eval(expr, envir = env)


## ----------------------------------------------------------------------
log_eval_expr <- function(expr, env = parent.frame()) {
  val <- eval(expr, envir = env)
  cat("Expression", deparse(expr), "evaluated to", val, "\n")
}

x <- 5
log_eval_expr(quote(x + 1))


## working substitute()
my_logger <- function(expr) {
  code <- substitute(expr)
  cat("You typed: ", deparse(code), "\n")
  eval(code)
}

my_logger(x + 1)

## ----------------------------------------------------------------------
# does not need substitute()... alreaduy and expression
code <- quote(x + 1)
eval(code, envir = env)



## ----------------------------------------------------------------------
# error: true
get_and_print <- function(name, env = parent.frame()) {
  if (!exists(name, envir = env)) stop("Boo! it is not here")
  val <- get(name, envir = env)
  print(val)
}

e <- rlang::env(x = 100)
e2 <- rlang::env(y = 2)
x <- 10
get_and_print("x") # 10
get_and_print("x", env = e) # 100
get_and_print("x", env = e2) # 10 again because of the env chain
get_and_print("zz", env = e2) # error trigger by stop()


## Controlled Lookup in Logging
log_value <- function(varname) {
  # Capture the name (unevaluated)
  name <- substitute(varname)

  # Deparse to string
  var_str <- deparse(name)

  # Lookup the value
  value <- get(var_str, envir = parent.frame())

  cli::cli_inform("Variable {.code {var_str}} has value: {value}")
}

score <- 88
log_value(score)


## assign
assign("z", 999)
z
z <- 999

## ----------------------------------------------------------------------
e <- rlang::env()
assign("x", 123, envir = e)
e$x

identical(
quote(x + 1) |> as.character(),
quote(x + 1) |> deparse()
)
d <- quote(x + 1)

## ----------------------------------------------------------------------
set_and_show <- function(name, value) {
  name_str <- deparse(substitute(name))
  env <- rlang::env()

  assign(name_str, value, envir = env)

  # notice that we are extracting the value using `get()` not `value`

  cli::cli_inform("Created {.code {name_str}} with value {.val {get(name_str, env)}} using {.code get()}")
}


## ----------------------------------------------------------------------
set_and_show(sdsds, 75)


## {rlang} Equivalents: env_get(), env_poke(), and Friends
e <- rlang::env(x = 42)
rlang::env_get(e, "x")


## ----------------------------------------------------------------------

# error. No chain
y <- 5
rlang::env_get(e, "y")
rlang::env_get(e, "y", default = NA)
rlang::env_get(e, "y", inherit = TRUE)


## ----------------------------------------------------------------------
# error: true
get_and_print_rlang <- function(name,
                          env = parent.frame(),
                          inherit = FALSE) {
  if (!exists(name, envir = env)) stop("Boo! it is not here")
  val <- rlang::env_get(env = env,
                        nm = name,
                        inherit = inherit)
  print(val)
}

env2 <- rlang::env(y = 2)
x <- 10
get_and_print_rlang("x") # 10
get_and_print_rlang("x", env = env) # 100
get_and_print_rlang("x", env = env2) # error
get_and_print_rlang("x", env = env2, TRUE) # 10 again because of the env chain


## exists(), assign(), and rm() alternatives
e <- rlang::env()
rlang::env_poke(e, "z", 100)
e$z


## ----------------------------------------------------------------------
rlang::env_has(e, "z")


## ----------------------------------------------------------------------
rlang::env_unbind(e, "z")
rlang::env_has(e, "z")



## Data masking
log_expr_base <- function(expr) {
  print(substitute(expr))
}
log_expr_base(x + 1)

# But, substitute() has no built-in way to capture quosures — expressions plus
# their environment.

##
log_expr <- function(expr) {
  quo <- rlang::enquo(expr)
  print(quo)
}

quo <- log_expr(x + 1)


## eval_tidy
data <- list(x = 1:5)
env  <- rlang::env(threshold = 3)

expr <- quote(x > threshold)
# This works: x from data, threshold from env
right <- rlang::eval_tidy(expr, data = data, env = env)


# This fails: base R's eval() only sees data, not env
# and threshold is not in global
eval(expr, envir = data)
eval(expr, envir = env)

# Yet, if we had a threshold in global, we would get
# the wrong results.
threshold <- 2
wrong <- eval(expr, envir = data)

all.equal(right, wrong)

data <- list(x = 1:5,
             threshold = 4)

# will look first in data
rlang::eval_tidy(expr, data = data, env = env)
rlang::eval_tidy(expr, data = data)



## example of function
my_filter <- function(data, expr) {
  expr <- substitute(expr)
  rows <- rlang::eval_tidy(expr, data)
  data[rows, ]
}


## ----------------------------------------------------------------------
my_filter(mtcars, mpg > 25)

threshold <- 200
my_filter(mtcars, hp > threshold)


##
my_filter_eval <- function(data, expr) {
  expr <- substitute(expr)
  rows <- eval(expr, envir = data)
  data[rows, ]
}

my_filter_eval(mtcars, mpg > 25)
my_filter_eval(mtcars, hp > threshold)
# same thing


## how eval differs from eval_tidy()
fruit <- "apple"  # global
data <- list(fruit = "banana")  # passed data


## ----------------------------------------------------------------------
with_data <- function(data, expr) {
  fruit <- "avocado"  # local inside function
  quo <- rlang::enquo(expr)
  print(quo)
  rlang::eval_tidy(quo, data)
}


## ----------------------------------------------------------------------
with_data(data, fruit)
with_data(NULL, fruit)


## ----------------------------------------------------------------------
with_data_eval <- function(data, expr) {
  fruit <- "avocado"
  expr <- substitute(expr)
  eval(expr, envir = data)
}


## ----------------------------------------------------------------------
with_data_eval(data, fruit)
with_data_eval(NULL, fruit)



## return to get()
# error
dt <- data.frame(x = c(1:5))

# you need to pass a real enviroment. Otherwise it breaks
get("x", envir = dt)

# This works
get("x", envir = list2env(dt))



## Symbols from string
x <- 101   # layer 1
var <- "x" # layer 2
expr <- as.name(var) #layer 3
expr

expr2 <- quote(var) #layer 4
expr2 # too literal

eval(expr)  # same as eval(quote(x))
eval(expr2)  # layer 2


## super substitute
var <- "hp"
threshold <- 200
expr <- substitute(v > t, list(v = as.name(var),
                               t = threshold))
expr
eval(expr, envir = mtcars)


## ----------------------------------------------------------------------
x <- 101
var <- "x"
sym <- rlang::sym(var) # like as.name()
sym
eval(sym)  # equivalent to eval(quote(x))


## ----------------------------------------------------------------------
expr <- substitute(v > t, list(v = rlang::sym(var),
                               t = threshold))
expr
eval(expr, envir = mtcars)


## ----------------------------------------------------------------------
expr_list <- parse(text = "hp > 200")
length(expr_list)
expr_list[[1]]  # You have to extract the first element
eval(expr_list[[1]], envir = mtcars)


## ----------------------------------------------------------------------
expr <- rlang::parse_expr("hp > 200")
expr
eval(expr, envir = mtcars)



## ----------------------------------------------------------------------
filter_by_name_base <- function(data, varname, cutoff) {
  expr <- substitute(v > t, list(v = as.name(varname), t = cutoff))
  rows <- eval(expr, envir = data)
  data[rows, ]
}
filter_by_name_base(mtcars, "hp", 200)


## ----------------------------------------------------------------------
filter_by_name <- function(data, varname, cutoff) {
  var_sym <- rlang::sym(varname)
  expr <- substitute(v > t, list(v = as.name(varname), t = cutoff))
  rows <- rlang::eval_tidy(expr, data)
  data[rows, ]
}

filter_by_name(mtcars, "hp", 200)


## simplify super substitute() --> !!
var <- "hp"
cutoff <- 200

expr <- substitute(v > t, list(v = as.name(var), t = cutoff))
expr
eval(expr, envir = mtcars)


## ----------------------------------------------------------------------
var <- rlang::sym("hp")
cutoff <- 200

expr <- rlang::expr(!!var > !!cutoff)
expr
rlang::eval_tidy(expr, data = mtcars)


## ----------------------------------------------------------------------
filter_tidy <- function(data, varname, cutoff) {
  var_sym <- rlang::sym(varname)
  expr <- rlang::expr(!!var_sym > !!cutoff)
  rows <- rlang::eval_tidy(expr, data)
  data[rows, ]
}

filter_tidy(mtcars, "hp", 200)


## !!!
args <- list(x = quote(x), na.rm = TRUE)
call <- as.call(c(quote(mean), args))
call
eval(call, envir = data.frame(x = c(1, 2, NA)))


## ----------------------------------------------------------------------
args <- list(x = rlang::sym("x"), na.rm = TRUE)

expr <- rlang::expr(mean(!!!args))
expr

rlang::eval_tidy(expr, data = data.frame(x = c(1, 2, NA)))


## defensive programing
#| error: true
try({
bad_filter <- function(data, var, cutoff) {
  expr <- rlang::expr(!!rlang::sym(var) > !!cutoff)
  rows <- rlang::eval_tidy(expr, data)
  data[rows, ]
}

bad_filter(mtcars, "mpg", 25)  # Works
bad_filter(mtcars, "not_a_column", 25)  # Breaks with cryptic error
})


## ----------------------------------------------------------------------
#| error: true
try({
safe_filter <- function(data, var, cutoff) {
  if (!var %in% names(data)) {
    rlang::abort(
      message = glue::glue("Variable '{var}' not found in data."),
      class = "invalid_column"
    )
  }

  expr <- rlang::expr(!!rlang::sym(var) > !!cutoff)
  rows <- rlang::eval_tidy(expr, data)
  data[rows, ]
}

safe_filter(mtcars, "gear", 4)
safe_filter(mtcars, "not_a_column", 4)  # Now fails gracefully
})


## ----------------------------------------------------------------------
#| error: true
try({
default_filter <- function(data, var = NULL, cutoff = 0) {
  var <- if (is.null(var)) {
    "mpg"  # fallback to "mpg" if NULL
  } else {
    var
  }

  if (!var %in% names(data)) {
    rlang::abort(
      message = glue::glue("Variable '{var}' not found in data."),
      class = "invalid_column"
    )
  }

  expr <- rlang::expr(!!rlang::sym(var) > !!cutoff)
  rows <- rlang::eval_tidy(expr, data)
  data[rows, ]
}

default_filter(mtcars, "gear", 4)
default_filter(mtcars, "not_a_column", 4)  # Now fails gracefully
})


## ----------------------------------------------------------------------
#| error: true
try({
robust_filter <- function(data, var, cutoff) {
  expr <- rlang::expr(!!rlang::sym(var) > !!cutoff)

  tryCatch(
    {
      rows <- rlang::eval_tidy(expr, data)
      data[rows, ]
    },
    error = function(e) {
      rlang::abort(
        glue::glue("Filtering failed for column '{var}':
                   {e$message}"),
        class = "filter_error"
      )
    }
  )
}
robust_filter(mtcars, "gear", 4)
robust_filter(mtcars, "not_a_column", 4)  # Now fails gracefully
})


## ----------------------------------------------------------------------
smart_filter <- function(data, var, cutoff) {
  # Capture the input
  quo <- rlang::enquo(var)
  expr <- rlang::quo_get_expr(quo)

  # Determine symbol from input
  if (rlang::is_symbol(expr)) {
    var_sym <- expr
  } else if (rlang::is_string(expr)) {
    var_sym <- rlang::sym(expr)
  } else {
    rlang::abort("`var` must be a column name (unquoted) or a string.")
  }

  # Check if column exists
  if (!as.character(var_sym) %in% names(data)) {
    rlang::abort(glue::glue("Column '{var_sym}' not found in data."))
  }

  # Build and evaluate the expression
  expr <- rlang::expr(!!var_sym > !!cutoff)
  rows <- rlang::eval_tidy(expr, data)
  data[rows, ]
}



## ----------------------------------------------------------------------
smart_filter(mtcars, mpg, 25)         # unquoted NSE
smart_filter(mtcars, "mpg", 25)       # quoted string
smart_filter(mtcars, var = "mpg", 25) # still works

