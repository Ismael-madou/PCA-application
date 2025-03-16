install.packages("robotstxt")
library (robotstxt)
install.packages("rvest")
library(rvest)
library(string)
library(dplyr)
library(purrr)


options(robotstxt_warn = FALSE)

paths_allowed(
  paths  = c("https://lefootenbref.com/chapionnats-europeens/premier-league/"),
  domain = "lefootenbref",
  bot    = "*"
)

options(robotstxt_warn = FALSE)

rtxt <- robotstxt(domain = "lefootenbref.com")

rtxt$check(
  paths = c("https://lefootenbref.com/chapionnats-europeens/premier-league/"),
  bot   = "*"
)

rt_req <- attr(rt, "request")
rt_req$request
rt_req$all_headers
as.list(rt_req)


install.packages("rvest")
library(rvest)
page <- read_html("https://lefootenbref.com/chapionnats-europeens/premier-league/")
article_url <- page %>% html_elements("h3") %>% html_elements("a") %>% html_attr("href")
article


options(robotstxt_warn = FALSE)

paths_allowed(
  paths  = c("https://lefootenbref.com/chapionnats-europeens/premier-league/"),
  domain = "lefootenbref",
  bot    = "*"  
)

