table <- sqlQuery("select * from projections where district = 'Ashland' and year = 2024", 'salary')

last <- table %>% 
  dplyr::filter(salary !=0) %>% 
  dplyr::group_by(col_num) %>% 
  dplyr::filter(row_num == max(row_num)) %>% 
  dplyr::pull(fte)

temp <- tibble::tibble()
for(i in unique(table$col_num)){
  adv <- table %>% 
    dplyr::filter(salary !=0 & col_num == i) %>% 
    dplyr::mutate(fte = dplyr::lag(fte, default = 0))
  
  adv[nrow(adv), 7] = adv[nrow(adv), 7]+last[i]
  temp <- dplyr::bind_rows(temp, adv)
}

new <- temp %>% 
  dplyr::select(col_num, row_num, fte_n1 = fte)

dplyr::left_join(table, new, by = c('col_num', 'row_num')) %>% 
  dplyr::mutate(fte = fte_n1) %>% 
  dplyr::select(-fte_n1)


scales <- sqlQuery("select * from user_scales", 'salary')

reshape2::acast(scales, row_num ~ col_num, function(x) {sort(as.character(x))[1]},
      value.var = 'salary', fill = '0') %>% 
  tibble::as_tibble() %>% 
  dplyr::rename_with(function(x)paste('Year 1 - Lane', x))

reshape2::acast(scales, row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                value.var = 'fte', fill = '0')%>% 
  tibble::as_tibble()












