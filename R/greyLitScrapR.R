#' Get all articles from a given page URL
#'
#' @description
#' Gets the titles, links, and dates of individual articles
#' from a given page URL
#'
#' @param url the URL containing the article lists
#' @param xpath_title the xpath to obtain titles
#' @param xpath_link the xpath to obtain links
#' @param xpath_date the xpath to obtain dates
#'
#' @return a dataframe containing the page URL
#' The titles, and the links
#' \preformatted{
#' data,.frame(page_url, titles, links)
#' }
#' @export
get_articles_from_page <- function(
  url,
  xpath_title = NULL,
  xpath_link = NULL,
  xpath_date = NULL,
  session = NULL
) {
  scrape <- scrape_url(url, session)
  titles <- NA
  links <- NA
  dates <- NA
  if (!is.null(xpath_title)) {
    titles <- get_elem_value(scrape$html, xpath_title)
  }
  if (!is.null(xpath_link)) {
    links <- get_elem_value(scrape$html, xpath_link)
  }
  if (!is.null(xpath_date)) {
    dates <- get_elem_value(scrape$html, xpath_date)
  }
  return(data.frame(title = titles, link = links, date = dates))
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
#' @export
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

#' Get all articles from a given site
#'
#' @description
#' Gets the titles, links, and dates of individual articles
#' from a given site
#'
#' @param base_url the URL containing up to the page number
#' @param first_page the first page
#' @param last_page the last page
#' @param xpath_title the xpath to obtain titles
#' @param xpath_link the xpath to obtain links
#' @param xpath_date the xpath to obtain dates
#' @param session optional session (from [polite::bow()]) to reuse
#'
#' @seealso [get_page_number()] to extract first and last page numbers
#' @return a dataframe containing the page URL
#' The titles, and the links
#' \preformatted{
#' data,.frame(page_url, titles, links)
#' }
#' @export
get_all_articles_from_site <- function(
  base_url,
  first_page = 1,
  last_page = 1,
  xpath_title = NULL,
  xpath_link = NULL,
  xpath_date = NULL,
  session = NULL
) {
  all_articles <- data.frame(
    title = character(),
    link = character(),
    date = character()
  )
  all_page_urls <- paste0(
    base_url,
    first_page:last_page
  )
  for (url in all_page_urls) {
    all_articles <- rbind(all_articles, get_articles_from_page(url,
      xpath_title = xpath_title,
      xpath_link = xpath_link,
      xpath_date = xpath_date,
      session = session
    ))
  }
  return(all_articles)
}