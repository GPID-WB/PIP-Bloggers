# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project:       PIP Example for Conditions Adv R
# Author:        Diana C. Garcia Rojas
# Dependencies:  The World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    Feb 2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load Libraries and functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(rlang)

# Function for logging
add_log <- function(cnd) {
  cat(
    "[", class(cnd)[[1]], "-", class(cnd)[[2]], "] ",
    cnd$message," for ",
    cnd$link, "\n", sep = "",
    file = "log.txt", append = TRUE
  )
}

# Function to handle duplicated observations in pfw
unq_pfw <- function(dt,
                    keyVar,
                    log_err = TRUE,
                    skip_err = TRUE){

  if(data.table::uniqueN(dt, by = keyVar) != nrow(dt)){

    tryCatch(
      expr = {

        dt_d <- dt[duplicated(dt, by = keyVar)]
        n_rep <- nrow(dt_d)
        cli::cli_abort(message = "There {?is/are} {n_rep} duplicates in `pfw`",
                       class = c("dup_pfw", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(dt_d$link),
                       call = sys.call())
      },
      dup_pfw = function(cnd){

        if(cnd$log){
          # Log the error
          add_log(cnd)
        }

        if(!cnd$skip){
          # Abort if you don't want to skip, but after logging
          cli::cli_abort(cnd$message, call = cnd$call)
        }

      },

      finally = {

        dt <- unique(dt, by = keyVar)

      }
    )
  }
  return(dt)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load data  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pfw <- pipload::pip_load_aux("pfw")

keyVar <- c("country_code",
            "surveyid_year",
            "survey_acronym")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               Duplicate one value    ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pfw_d <- rbind(pfw, pfw[rep(1, 5), ])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Use function  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pfw_f <- unq_pfw(pfw_d, keyVar)

