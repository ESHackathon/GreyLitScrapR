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
  return(list(page_url = url, titles = titles, links = links, dates = dates))
}