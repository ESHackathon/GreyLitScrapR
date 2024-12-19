#' politely scrape a URL
#'
#' @description
#' Scrapes a URL politely, optionally utilising an existing session
#'
#' @param url The URL to scrape
#' @param session the session to use, defaulting to no existing session
#'
#' @return a list containing the scraped URL, and the utilised session
#' \preformatted{list(html, session)}
#' @seealso [polite::bow()] to initialise a session,
#' [polite::nod()] to change the URL of an existing session,
#' and [polite::scrape()] to scrape the HTML
#'
#' @keywords internal
scrape_url <- function(url, session = NULL) {
  if (is.na(session) || is.null(session)) {
    session <- polite::bow(url)
  } else {
    session <- polite::nod(session, url)
  }
  html <- polite::scrape(session)
  return(list(html = html, session = session))
}

#' Get page numbers from the URL
#'
#' @description
#' Gets page numbers from a site
#'
#' @param url The URL to scrape
#' @param xpath_page_url the xpath to extract the page URL
#' @param regex_page_number the regex to extract the page number from
#' the returned URL
#' @param session the session to use, defaulting to no existing session
#'
#' @return a list containing the base URL,
#' the page number (to append to the base URL),
#' and the full page URL
#' \preformatted{list(
#'    base_url = base_url,
#'    page_number = page_number,
#'    page_url = page_url,
#'    session = scrape$session
#'  }
#'
#' @keywords internal
get_page_number <- function(
  url,
  xpath_page_url,
  regex_page_number,
  session = NULL
) {
  scrape <- scrape_url(url, session)
  page_url <- scrape$html %>%
    rvest::html_element(xpath = xpath_page_url) %>%
    rvest::html_text2()
  print(page_url)
  page_number <- as.numeric(
    stringi::stri_extract_last_regex(
      page_url, regex_page_number
    )
  )
  base_url <- gsub(
    page_number,
    "",
    stringi::stri_extract_first_regex(
      page_url,
      paste0(".+?", regex_page_number)
    )
  )

  return(
    list(
      base_url = base_url,
      page_number = page_number,
      page_url = page_url,
      session = scrape$session
    )
  )
}

#' gets an element value
#'
#' @description
#' Gets the value of an element from a given xpath
#'
#' @param html the html to search
#' @param xpath the xpath to use
#'
#' @return a list containing all the given values
#' \pre
#'
#' @keywords internal
get_elem_value <- function(html, xpath) {
  value <- html %>%
    rvest::html_elements(xpath = xpath) %>%
    rvest::html_text(trim = TRUE)
  return(value)
}