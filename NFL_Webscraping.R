#NFL Webscrapping
WebScraping <- function(URL, CSS) {
  #read_html  -- Reading the HTML code from the website
  #html_nodes -- Using CSS selectors to scrape the rankings section
  #html_text  -- Converting the ranking data to text
  html_text(html_nodes(read_html(URL), CSS))
}