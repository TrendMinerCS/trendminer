## ----setup, include = FALSE---------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = NOT_CRAN,
  eval = NOT_CRAN
)

## ----eval=FALSE---------------------------------------------------------------
#  token <- tm_token(client_id = "YOUR_CLIENT_ID_HERE",
#             client_secret = "YOUR_CLIENT_SECRET_HERE",
#             usr_name = "YOUR_USER_NAME_HERE",
#             usr_pwd = "YOUR_USER_PASSWORD_HERE",
#             base_url = "YOUR_TM_BASE_URL_HERE")

## ----eval=FALSE---------------------------------------------------------------
#  TM_client_ID = YOUR_CLIENT_ID_HERE
#  TM_client_secret = YOUR_CLIENT_SECRET_HERE
#  TM_usr = YOUR_USER_NAME_HERE
#  TM_pwd = YOUR_USER_PASSWORD_HERE
#  TM_base_url = YOUR_TM_BASE_URL_HERE

## -----------------------------------------------------------------------------
library(trendminer)

token <- tm_token()

## ----include=FALSE------------------------------------------------------------
access_token <- token$access_token
tm_url <- token$base_url
token$access_token <- "YOUR_ACCESS_TOKEN_WOULD_BE_SHOWN_HERE"
token$base_url <- "YOUR_TM_BASE_URL_WOULD_BE_SHOWN_HERE"

## -----------------------------------------------------------------------------
token

## ----include=FALSE------------------------------------------------------------
token$access_token <- access_token
token$base_url <- tm_url

## ----message=FALSE------------------------------------------------------------
library(dplyr)

tm_af_search_assets(token, 'name=="*Reactor*"') %>% 
  select(nodeId, name, type, tagName) %>%
  head()

## -----------------------------------------------------------------------------
tm_af_search_assets(token, "type=='ATTRIBUTE';name=='Temperature*'") %>% 
  select(nodeId, name, type, tagName) %>% 
  head()

## -----------------------------------------------------------------------------
assets <- tm_af_assets(token)
dim(assets)
assets %>% 
  select(nodeId, name, type) %>% 
  head()

tags <- tm_af_tags(token)
dim(tags)
tags %>% 
  select(nodeId, name, type, tagName) %>% 
  head()

## -----------------------------------------------------------------------------
roots <- tm_af_root_structures(token) 
roots %>% 
  select(structureId, name, nodeId, type)

## -----------------------------------------------------------------------------
tm_af_child_structures(token, roots$structureId[roots$name == "Site Barcelona"]) %>% 
  select(structureId, nodeId, name, path, type, parentName)

## -----------------------------------------------------------------------------
solvents_str <- tm_af_descendant_structures(token,"1aef0aa1-f942-441e-82d8-0c4bfe7208b3")
solvents_str %>% 
  select(name, parentName, type, tagName)

## ----message=FALSE------------------------------------------------------------
library(data.tree)

solvents_subtree <- FromDataFrameTable(solvents_str, pathName = "externalId") 
print(solvents_subtree, "type", "tagName")

## ----message=FALSE------------------------------------------------------------
library(networkD3)

diagonalNetwork(ToListExplicit(solvents_subtree$Solvents, unname = TRUE), fontSize = 14,
                height = 400, width = 600)

## -----------------------------------------------------------------------------
library(purrr)

plots <- tm_af_root_structures(token) %$%
  map(structureId, tm_af_descendant_structures, token = token) %>%
  map(FromDataFrameTable, pathName = "externalId") %>%
  map(~ diagonalNetwork(ToListExplicit(.x, unname = TRUE), fontSize = 14, 
                        height = 500, width = 700))

## -----------------------------------------------------------------------------
plots[[1]]

## -----------------------------------------------------------------------------
plots[[2]]

## -----------------------------------------------------------------------------
plots[[3]]

## -----------------------------------------------------------------------------
reactor1_str <- tm_af_child_structures(token,"5a0d1bf8-f298-4946-b523-8dcd5194fbaf")
reactor1_str %>% 
  select(parentName, name, type, tagName)

## ----message=FALSE------------------------------------------------------------
library(lubridate)

start <-  ymd_hms("2019-09-15T03:10:14Z")
end <- ymd_hms("2019-09-15T08:42:15Z")

ba_conc1_data <- tm_ts_interpolated_data(token, "BA:CONC.1", start, end, step = 2)


## -----------------------------------------------------------------------------
str(ba_conc1_data)
head(ba_conc1_data$timeSeries)
summary(ba_conc1_data$timeSeries)

## ----single_ts_plot, message=FALSE, fig.height=3, fig.width=7, fig.align="center"----
library(ggplot2)

ba_conc1_data$timeSeries %>%
  ggplot(aes(index, value)) +
  geom_line(color = "#09557f") +
  ggtitle(ba_conc1_data$tag$tagName) +
  theme_minimal()

## -----------------------------------------------------------------------------
tags <- reactor1_str$tagName
tags <- tags[1:3]
tags

combined_data <- map(tags, ~ tm_ts_interpolated_data(token, .x,  start, end, 2)) %>%
  map_dfr( ~ .x[["timeSeries"]] %>%
             mutate(
               tag_name = .x[["tag"]][["tagName"]]
              )) 

## ----ggplot_mulitvariate_ts, fig.height=3, fig.width=7, fig.align="center"----
combined_data %>% 
  ggplot(aes(index, value)) + 
  geom_line(aes(color = tag_name)) +
  ggtitle("Reactor 1 tags")+
  theme_minimal()

