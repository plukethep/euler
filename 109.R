library(tidyverse)
library(magrittr)
#rep(1:20, each=3)

# make all possible scores on darts board
core <- c(1:20)
darts <- expand.grid(c("S", "D", "T"), core) %>% 
  as.data.frame() %>% 
  rename(type = Var1, area = Var2) %>%
  mutate(points = area * rep(c(1,2,3),20)) %>% 
  mutate(finish = !type %in% c("S", "T")) %>%
  add_row(type="S", area=25, points=25, finish=F) %>%
  add_row(type="D", area=50, points=50, finish=T) %>%
  mutate(throw = paste0(type, area))

# for one and two dart finishes
dart_out <- function(score=40){
  
  out <- data.frame()
  # single dart
  out <- rbind(out, darts %>% filter(finish, points == score) %>% 
                 mutate(throw1 = throw, 
                        throw2 = NA_character_, 
                        throw3 = NA_character_) %>%
                 select(matches("throw[0-9]"))) 
  
  # double dart
  first <- darts %>% filter(points < score)
  out <- rbind(out, 
               map_dfr(first$throw, function(x){
                        scr  <- score - darts %>% filter(throw == x) %$%
                          points
                        darts %>% filter(finish, points == scr) %>% 
                          mutate(throw1 = x) %>%
                          rename(throw2 = throw) %>%
                          select(throw1, throw2) %>%
                          mutate(throw3 = NA_character_)
                      }))
  
  out %>% mutate(score = score)
}

# get one and two darts
out <- map_dfr(1:99, function(x){
  # x <- 20
  message(x)
  return(dart_out(x))
})

# three darts
three <- map_dfr(1:99, function(x){
  message(x)
  first <- darts %>% filter(points < x)
  first <- first %>% 
    mutate(remaining = x - points) %>% 
    left_join(out, by=c("remaining"="score")) %>%
    mutate(throw3 = throw2,
           throw2 = throw1,
           throw1 = throw,
           score = x) %>%
    select(throw1, throw2, throw3, score) %>%
    filter(!is.na(throw2), !is.na(throw3))

  return(first)
})

all_darts <- rbind(three, out) %>% distinct() %>% arrange(score)

all_darts <- all_darts %>% 
  filter(!is.na(throw3)) %>% 
  mutate(combo = ifelse(throw1 > throw2, 
                        paste0(throw1,";", throw2),
                        paste0(throw2,";", throw1))) %>%
  select(combo, throw3, score) %>%
  distinct() %>%
  mutate(throw1 = str_extract(combo, ".*(?=[;])"),
         throw2 = str_extract(combo, "(?<=[;]).*")) %>%
  select(throw1, throw2, throw3, score) %>%
  rbind(all_darts %>% filter(is.na(throw3)))
  
nrow(all_darts)
