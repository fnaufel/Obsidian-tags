

get_md <- function(file) {
  
  parsermd::parse_rmd(file, allow_incomplete = TRUE) 
  
}


get_frontmatter <- function(md) {
  
  frontmatter <- md %>% 
    parsermd::rmd_select(parsermd::has_type('rmd_yaml_list'))
  
  # There is only one YAML block, so 
  frontmatter %>% pluck(1)

}

get_body <- function(md) {
  
  md %>% 
    parsermd::rmd_select(-parsermd::has_type('rmd_yaml_list'))
  
}

get_tags <- function(fm) {
  
  fm %>% pluck('tags')
  
}

get_areas <- function(fm) {
  
  fm %>% pluck('areas')
  
}

area2tag <- function(area, add_hash = TRUE) {
  
  area %>% 
    # Remove link chars
    stringr::str_remove('^[ ]*\\[\\[') %>% 
    stringr::str_remove('\\]\\][ ]*$') %>% 
    # Remove alias
    stringr::str_remove('\\|.*$') %>% 
    # Remove leading and trailing spaces
    stringr::str_trim() %>% 
    # Replace remaining spaces with -    
    stringr::str_replace_all('[ ]+', '-') %>% 
    # Make lowercase
    stringr::str_to_lower() %>% 
    paste0(
      ifelse(add_hash, '#', ''), .
    )
  
}


set_tags <- function(fm, tags) {
  
  fm %>% 
    modify_at('tags', ~tags)
  
}

remove_areas <- function(fm) {
  
  fm %>% 
    discard_at('areas')
  
}


# Return string
fix_frontmatter <- function(fm) {

  areas <- fm %>% get_areas()  
  tags <- fm %>% get_tags()  
  
  if (is.null(areas))
    return(fm)
  
  areas <- area2tag(areas, FALSE)
  tags <- union(tags, areas)
  
  fm %>% 
    set_tags(tags) %>% 
    remove_areas() %>% 
    as.yaml() %>% 
    paste0('---\n', ., '---\n\n')
}


fix_body_part <- function(ast) {

  area_regex <- '\\[\\[Areas/[^]]+\\]\\]'
  
  ast %>% 
    as_document() %>% 
    str_replace_all(area_regex, area2tag)

}


fix_body <- function(body) {
  
  body %>% 
    as_tibble() %>% 
    pmap(
      \(sec_h1, sec_h2, type, label, ast) {
        
        if (type == 'rmd_heading') {
          # Add \n
          paste0(fix_body_part(ast), '\n')
        } else if (type == 'rmd_markdown') {
          # No need to add \n
          fix_body_part(ast)
        } else {
          ast
        }
        
      }
    ) %>% 
    map_chr(paste0, collapse = '\n') %>% 
    paste0(collapse = '\n')
  
}


# Returns string
fix_file <- function(file) {
  
  md <- get_md(file)
  fm <- get_frontmatter(md) %>% fix_frontmatter()
  body <- get_body(md) %>% fix_body()
  
  paste0(fm, body, collapse = '\n')
  
}


# Fix gets dirname or filename as only arg
# It processes all files in the dir and overwrites them
# 
fix <- function(dir_or_file) {
  
  NULL
  
}
