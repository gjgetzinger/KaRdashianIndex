#' Search for Google Scholar ID by name and affiliation
#'
#' @param first_name Researcher first name.
#' @param last_name Researcher last name.
#' @param affiliation Researcher affiliation.
#'
#' @return Google Scholar ID as a character string.
#' @export
#'
#' @examples
#'  get_scholar_id(first_name = "gordon", last_name = "getzinger")
#'
#'
get_scholar_id <- function(first_name, last_name, affiliation = NA) {
  url <- paste0(
    'https://scholar.google.com/citations?view_op=search_authors&mauthors=',
    first_name,
    '+',
    last_name,
    '&hl=en&oi=ao'
  )
  aa <- RCurl::getURL(url = url)
  ids <-
    stringr::str_extract_all(string = aa, pattern = ";user=(.){1,13}")

  if (length(unlist(ids)) == 0) {
    message("No Scholar ID found.")
    return(NA)
  }

  ids <- ids %>%
    unlist %>%
    gsub(";user=|[[:punct:]]$", "", .) %>%
    unique

  if (length(ids) > 1) {
    profiles <- lapply(ids, scholar::get_profile)
    if (is.na(affiliation)) {
      x_profile <- profiles[[1]]
    } else {
      which_profile <- sapply(profiles, function(x) {
        stringr::str_count(
          string = x$affiliation,
          pattern = stringr::coll(affiliation, ignore_case = TRUE)
        )
      })
      if(all(which_profile == 0)){
        message("No researcher found at the indicated affiliation.")
        return(NA)
      } else {
        x_profile <- profiles[[which(which_profile != 0)]]
      }
    }
  } else {
    x_profile <- scholar::get_profile(id = ids)
  }
  return(x_profile$id)
}


#' Get the number of users for a given twitter user
#'
#' @param twitter_handle A twitter handle
#'
#' @return The number of users or NA when no user is found.
#' @export
#'
#'@examples
#'   n_followers(twitter_handle = "gjgetzinger")
#'
n_followers <- function(twitter_handle) {
  users <- rtweet::search_users(q = twitter_handle, n = 1)
  if(nrow(users)>0){
    users %>%
      dplyr::select(user_id) %>%
      dplyr::pull() %>%
      rtweet::get_followers(user = .) %>%
      data.frame %>%
      nrow
  } else {
    NA
  }
}

#' Calculate the Kardashian Index for a researcher
#'
#' https://en.wikipedia.org/wiki/Kardashian_Index
#'
#' @inheritParams n_followers
#' @inheritParams get_scholar_id
#'
#' @return The K-index of a researcher.
#' @export
#'
#' @examples
#' k_index(twitter_handle = "gjgetzinger", first_name = "gordon", last_name = "getzinger")
k_index <- function(twitter_handle,
           first_name,
           last_name,
           affiliation = NA) {
    id <-
      get_scholar_id(first_name = first_name,
                     last_name = last_name,
                     affiliation = affiliation)
    if(is.na(id)){
      message("No Scholar ID found")
      return(NA)
    } else {
      message(paste0("Google Scholar ID:", id))
    }
    prof <- tryCatch(expr = scholar::get_profile(id = id), error = function(e){
      message("No Google Scholar profile found")
      return(NA)
      })

    citations <- prof$total_cites
    message(paste0("Citations:", citations))

    foll_exp <- 43.3 * citations ^ (0.32)
    message(paste0("Followers Expected:", foll_exp))

    if(is.na(twitter_handle)){
      foll_act <- 0
      message("No twitter handle provided.")
    } else {
      foll_act <- n_followers(twitter_handle = twitter_handle)
      if(is.na(foll_act)){
        foll_act <- 0
        message("No twitter handle matching the provided input.")
      }
    }
    message(paste0("Followers Actual: ", foll_act))

    k <- foll_act/foll_exp
    message(paste0("Kardasian Index: ", k))
    if(k > 5){
      message("This researcher is a 'Science Kardashian' (K-index > 5)")
    }
    return(k)
  }


