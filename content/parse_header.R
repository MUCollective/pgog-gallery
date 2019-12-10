library(tidyverse)
library(commonmark)
library(xml2)
library(blogdown)
library(glue)


# helper

glue_tags <- function(tags)
  tags %>%
    str_split(", ") %>%
    unlist() %>%
    enframe() %>%
    mutate(value = glue("\"{value}\"")) %>%
    summarize(value = glue("[{toString(value)}]")) %>%
    .$value %>%
    as.character()


add_yaml_header <- function(one_md){
  
  f <- paste0(base_dir, one_md)
  
  f_xml <- f %>%
    readLines(encoding = "UTF-8") %>%
    blogdown:::split_yaml_body()  %>%
    .$body %>%
    commonmark::markdown_xml() %>%
    xml2::read_xml() %>%
    xml_ns_strip()
  
  title <- f_xml %>%
    xml_find_first(".//heading") %>%
    xml_text() %>%
    map(~paste("title", .x, sep = ": "))
  
  yaml_attrs <- f_xml %>%
    xml_find_first(".//paragraph")  %>%
    as_list()
  
  yaml_attrs <- c(title, yaml_attrs) %>% 
    unlist() %>%
    enframe() %>%
    select(value) %>%
    separate(value, into = c("attr", "value"), sep = ": ", extra = "merge") %>%
    mutate(attr = tolower(attr))

  # parse multiple tags to `["tag1", "tag2"]`
  yaml_attrs %>%
    pivot_wider(names_from = attr, values_from = value) %>%
    mutate(created = if (exists("created", where = .)) 
      as.character(as.Date(created, "%b %d, %Y %l:%M %p")) else "[\"\"]") %>% # parse to date
    mutate(coverage = if (exists("coverage", where = .))
             as.character(glue("\"{coverage}\"")) else "[\"\"]") %>%
    mutate(tags = if (exists('tags', where=.)) glue_tags(tags) else "[\"\"]") %>% 
    mutate(date = as.character(glue("\"{created}\"")), created = NULL) %>%
    mutate(title = as.character(glue("\"{title}\""))) %>%
    pivot_longer(everything(), names_to = "attr", values_to = "value") %>%
    unite(yaml, 1:2, sep = ": ")  %>%
    add_row(yaml = "---")  %>%
    add_row(yaml = "---", .before =1) %>%
    write.table(paste0(base_dir, one_md), row.names = FALSE, quote=FALSE, col.names = FALSE)
}

# main

base_dir <- "./Probabilistic visualizations/"

all_mds <- list.files(base_dir, recursive = TRUE, pattern = "*.md")

all_mds %>%
  enframe(name = NULL) %>%
  walk(.x = .$value,
       .f = ~ add_yaml_header(.x))

# https://ropensci.org/technotes/2018/09/05/commonmark/
