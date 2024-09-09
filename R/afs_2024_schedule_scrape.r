install.packages('rvest')
library(tidyverse)
library(rvest)
library(httr)
library(lubridate)
library(glue)
library(humaniformat)

base_url = "https://www.xcdsystem.com/afs/program/y7rKVr9/index.cfm"
## Functions ####
get_session_types = \(page) {
  session_pattern = '<i class="fa fa-folder-open-o" aria-hidden="true"></i>'
  page |> map_chr(as.character) |> str_split('\n') |>
    map_chr(\(x) {
      out =  str_subset(x, fixed(session_pattern)) |> 
         str_remove(fixed(session_pattern)) |> trimws() 
      if(length(out) != 1) out = NA_character_
      out
      })

}
parse_session = \(page) {
  metadata = page |> html_node('div.panel-primary')
  session_title = metadata |> html_node('h4') |> html_text2()
  metadata_info = metadata |> html_node('div.row') |> html_text2() |> str_split_fixed('\n', 4)
  # browser()
  meta_rows = tibble(session_name = session_title, session_date = metadata_info[1],
                     session_times = metadata_info[2], session_room = metadata_info[3]) |> 
    mutate(numeric_date = session_date |> str_remove(".+, ") |>  paste0(', 2024') |> mdy(),
           session_start = str_remove(session_times, ' - .+') |> hm())
  presentations = page |> html_nodes('div.panel-body:contains("\nPresentations\n")') |> 
    html_nodes('div.row.clickableRow') |> map_dfr(\(row) {
      # browser()
      times = row |> html_node("div.col-sm-2.text-center")  |> html_text2() |> str_replace('/n', ' - ') ##  |> str_replace('\n', ' - ') |>  trimws()
      title = row |> #html_nodes("div.col-sm-9") |> 
        html_node('div.program-item-title') |> html_text2()
      author_affil = row |> ##html_node("div.col-sm-9") |>
        html_node('div.row') |> html_text2() |> str_split('\n') |> unlist()
      abstract_id = row |> html_attr('data-abid')
      # if(is.na(times)) browser()
      tibble(times, title, author = author_affil[1], affiliation = author_affil[2], abstract_id)
    })
  
  presentations |> mutate(!!!meta_rows)
}

## Scrape the data ####
main_page  = read_html(base_url)

session_ids = main_page |>
  html_elements('li.list-group-session-item')  |> 
  html_elements('a') |> 
  html_attr('name')

session_urls = str_replace(session_ids, 'Sess', "https://www.xcdsystem.com/afs/program/y7rKVr9/index.cfm?pgid=1485&sid=") |> set_names(session_ids)
all_sessions = session_urls |> map(slowly(read_html)) # This takes a while
session_types = all_sessions |> get_session_types()
session_tbl = tibble(session_id = session_ids, session_type = session_types, session_html = all_sessions)
write_rds(session_tbl, "scrape/AFS2024_scrape.rds")

useful_sessions = session_tbl |> filter(session_types %in% c('Symposium', 'Poster', 'Contributed Oral Presentations'))

talk_info = useful_sessions |> 
  mutate(scraped_data = map(session_html, parse_session)) |>
  unnest(scraped_data) |> 
  mutate(times = str_replace(times, 'M', 'M -'))
talk_info = read_rds('scrape/afs_2024_talk_info.rds')
all_talks =  talk_info |> #select(author, affiliation, times, numeric_date, session_room, title, session_name,session_id, abstract_id) |> 
  mutate(author = str_remove(author, ',.+') |> str_remove_all(fixed('(')) |> str_remove_all(fixed(')')),
         first = first_name(author), last = last_name(author),
         print_date = numeric_date |> strftime('%a, %m/%d'),
         mdl = middle_name(author), mdl = if_else(is.na(mdl), '', paste0(' ', mdl)), 
         author_last = glue("{str_to_title(last)}, {str_to_title(first)}{str_to_title(mdl)}"),
         sessioN_id = str_remove(session_id, "Sess"),
         session_url = glue('https://www.xcdsystem.com/afs/program/y7rKVr9/index.cfm?pgid=1485&sid={session_id}'),
         title_url = glue('https://www.xcdsystem.com/afs/program/y7rKVr9/index.cfm?pgid=1485&sid={session_id}&abid={abstract_id}'),
         title_md = glue("[{title}]({title_url})"),
         session_md = glue("[{session_name}]({session_url})")) |> 
  mutate(times = case_when(
      !is.na(times) ~ times |> str_replace('\n', ' '),
      abstract_id == '124899' ~ '9:30 AM - 9:45 AM',
      session_type == 'Poster' ~ session_times
      ),
  start_time = times |> str_remove(' -.+'),
  end_time = times |> str_remove(".+- "),
  short_times = str_remove_all(times, ' (A|P)M')) |> 
  select(-session_html)

write_rds(all_talks, 'scrape/afs_2024_talk_info.rds')
