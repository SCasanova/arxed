
# Dependencies ------------------------------------------------------------


library(magrittr)
library(shiny)
options(encoding = "UTF-8")


# Connect to database -----------------------------------------------

sqlQuery <- function (query,db) {
  # creating DB connection object with RMysql package
  DB <- DBI::dbConnect(RMySQL::MySQL(), dbname = db, user = 'admin', password = 'ArxEd01742!',
                       host = 'arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com', 
                       port = 8209)
  # close db connection after function call exits
  on.exit(DBI::dbDisconnect(DB))
  # send Query to btain result set
  rs <- RMySQL::dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- RMySQL::fetch(rs, -1)
  # return the dataframe
  result
}


# Custom functions --------------------------------------------------------



year_progress <- function(scales){
  last <- scales %>% 
    dplyr::filter(salary !=0) %>% 
    dplyr::group_by(col_num) %>% 
    dplyr::filter(row_num == max(row_num)) %>% 
    dplyr::pull(fte)
  
  temp <- tibble::tibble()
  for(i in unique(scales$col_num)){
    adv <- scales %>% 
      dplyr::filter(salary !=0 & col_num == i) %>% 
      dplyr::mutate(fte = dplyr::lag(fte, default = 0))
    
    adv[nrow(adv), 5] = adv[nrow(adv), 5]+last[i]
    temp <- dplyr::bind_rows(temp, adv)
  }
  new <- temp %>% 
    dplyr::select(col_num, row_num, fte_n1 = fte)
  
  dplyr::left_join(scales, new, by = c('col_num', 'row_num')) %>% 
    dplyr::mutate(fte = fte_n1) %>% 
    dplyr::select(-fte_n1)
  
}

expand_grids <- function(salary, fte, user){
  tibble::tibble(
    user = user,
    col_num = expand.grid(seq_len(nrow(salary)), seq_len(ncol(salary)-1))[,2],
    row_num = expand.grid(seq_len(nrow(salary)), seq_len(ncol(salary)-1))[,1],
    salary = unlist(salary[seq_len(nrow(salary)), 2:ncol(salary)]) %>% 
      as.character() %>% 
      readr::parse_number(),
    fte = unlist(fte[seq_len(nrow(fte)), 2:ncol(fte)])
  ) %>% 
    dplyr::mutate(dplyr::across(salary:fte, function(x)ifelse(is.na(x), 0, x)))
}

spec_rename <- function(x){ #Renaming function
  purrr::map_chr(
    x, function(y){paste('Lane', as.numeric(strsplit(y,split="(?<=[a-zA-Z])\\s*(?=[0-9])",perl=TRUE)[[1]][2]) -1)}
  )
  
}

# Credential system for persistent data ---------------------------------------------


check_creds <- function(dbname, host, port, db_user, db_password) {
  
  function(user, password) {
    
    con <- DBI::dbConnect(RMySQL::MySQL(), 
                          dbname = dbname, 
                          user = db_user, 
                          password = db_password,
                          host = host, 
                          port = port)
    
    on.exit(DBI::dbDisconnect(con))
    
    
    res <- DBI::fetch(DBI::dbSendQuery(con, glue::glue_sql("SELECT * 
                            FROM userlist 
                            WHERE user = {user} 
                            AND password = {password}
                            ", user = user, password = password, .con = con)))
    
    if (nrow(res) > 0) {
      list(result = TRUE, user_info = list(user = user, something = 123))
    } else {
      list(result = FALSE)
    }
  }
}

ui <- shinyMobile::f7Page(
  # shinydisconnect::disconnectMessage(
  #    text = "Your session has timed out.",
  #    refresh = "Reload now",
  #    background = "#2a3a6e",
  #    size = 36,
  #    width = "full",
  #    top = "center",
  #    colour = "white",
  #    overlayColour = "#999",
  #    overlayOpacity = 0.4,
  #    refreshColour = "grey"
  #  ), 
  title = paste0("  LITIX Budget Forecasting"),  
  options = list(dark = F),
  tags$script(src = "myscript.js"),
  shinyMobile::f7SingleLayout(
    tags$head(
      tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Secondary%20Logo%20-%20Transparent%20BG.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
      tags$script(src = 'https://kit.fontawesome.com/8c5f048e9f.js',
                  crossorigin="anonymous" )
    ),
    navbar = shinyMobile::f7Navbar(
      title = tags$div(
        tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Secondary%20Logo%20-%20Transparent%20BG.png',
                 width='45px'),
        width='45px'),
      tags$span(h3(paste0("  LITIX Budget Forecasting")), 
                style = 'text-align:center;'), 
      tags$div(htmlOutput('logo_src'), class = 'top-district-logo')),
    tags$div(shinyMobile::f7Fab(
      inputId = 'pass',
      label = 'Change Password'
    ), class =  'pass-button'),
    #  tags$div(class = 'password-popup', ),
    shinyMobile::f7Tabs( #First tab for template/data upload
      animated = T,
      swipeable = F,
      id = "tabset",
      style = 'strong',
      
      
      
      # Home --------------------------------------------------------------------
      
      shinyMobile::f7Tab(
        tabName = "Data",
        title = "Home Page",
        icon = shinyMobile::f7Icon("arrow_up_doc"),
        # active = TRUE,
        tags$div(
          style = 'display:flex;',
          tags$div(
            style = 'width:50%;',
            shinyMobile::f7Shadow(
              intensity = 16,
              hover = T,
              shinyMobile::f7Card(
                tags$div(
                  style = "height:10vh;",
                  tags$div(
                    style = 'margin-bottom:1vh',
                    shinyMobile::f7Select(
                      inputId = 'steps',
                      label = tags$span('Number of steps', class = 'titles'),
                      choices = seq(0, 22),
                      selected = 0,
                      width = '100%'
                    )
                  ),
                  tags$div(
                    shinyMobile::f7Select(
                      inputId = 'lanes',
                      label = tags$span('Number of lanes', class = 'titles'),
                      choices = c( 0, seq(2, 13)),
                      selected = 0,
                      width = '100%'
                    )
                  )
                ),
                footer = tagList(tags$div(
                  style = "display:flex;" ,
                  tags$div(
                    style = "padding-right:5%;padding:2%" ,
                    shinyMobile::f7DownloadButton(
                      outputId = "sal_template",
                      label = "Salary Template",
                      color = 'gray'
                    ),
                    class = 'template-container'
                  ),
                  tags$div(
                    style = "padding-left:5%;padding:2%" ,
                    shinyMobile::f7DownloadButton(
                      outputId = "fte_template",
                      label = "FTE Template",
                      color = 'gray'
                    ),
                    class = 'template-container'
                  )
                )),
                tags$div(
                  class = 'extra-text',
                  tags$span('Important:', style = 'font-weight:800;font-size:120%'), 'Your current salary scales have already been loaded. If you wish to change them follow these steps:',
                  tags$ol(
                    tags$li('Select the appropriate number of lanes and steps'),
                    tags$li('Download and fill out the templates'),
                    tags$li('Upload the templates to the appropriate file managers (below)'),
                    tags$li('Click "Save" to make changes permanent')
                  ),
                  # tags$span(class = 'small-text', 'Note: The "optional" salary scales upload referrs to when there are two sets of scales due to negotiation')
                )
              )
            ),
            tags$div(
              shinyMobile::f7Shadow(
                intensity = 16,
                hover = T,
                shinyMobile::f7Card(
                  tags$div(
                    class = 'grid-buttons',
                    tags$div(
                      style = 'width:50%;',
                      shinyMobile::f7File("sal", 
                                          h3("Upload Salary Table"),
                                          width ='100%'),
                      shinyMobile::f7File("fte", h3("Upload FTE Table"),
                                          width ='100%')
                    )#,
                    # tags$div(
                    #   style = 'width:50%;margin-left:5vw;',
                    #   shinyMobile::f7File("sal_2", h3("Upload Salary Table 2 (Optional)"),
                    #                       width ='100%'),
                    #   shinyMobile::f7File("fac_2", h3("Upload FTE Table 2 (Optional)"),
                    #                       width ='100%')
                    # )
                  ),
                  footer = tagList(
                    tags$div(
                      shinyMobile::f7Button(
                        #calls for DB replacing
                        "save",
                        "Save",
                        outline = FALSE,
                        fill = TRUE,
                        shadow = FALSE,
                        rounded = FALSE,
                        size = NULL
                      ),
                      tags$span(class = 'tooltiptext', 
                                "Send this data combination to the server"),
                      class = 'card-button2 save-button',
                      style = 'width:30%;right:2%'
                    )
                  )
                )
              )
            )),
          tags$div(
            style = 'width:45%',
            tags$div(
              class = 'out_text',
              htmlOutput('custom_title'),
              textOutput('payroll')
            ),
            tags$div(
              class = 'out_text',
              tags$div(class = 'text-1',
                       "FTE Employees:"),
              textOutput('employees')
            ),
            tags$div(
              class = 'out_text',
              tags$div(class = 'text-1',
                       "% FTE Highest Step:"),
              textOutput('last_step')
            )
          )
        )
        
      ),
      
      # Results 3 ---------------------------------------------------------------
      shinyMobile::f7Tab( #results tab displaying plots and tables
        tabName = "Results",
        title = "3-Year Proj.",
        icon = shinyMobile::f7Icon("chart_bar"),
        # # active = TRUE,
        tags$div(
          style = 'width:35%;',
          tags$div(
            style = 'display:flex;',
            tags$div(class = "stepper-fifty",
                     tags$small(h5(
                       "Estimated Stipend % of Total Salaries"
                     )),
                     tags$div(
                       tags$input(id = 'stipends', 
                                  type = "number", 
                                  placeholder = "Stipend %", 
                                  value = "0.0", 
                                  min = '0', 
                                  max = '10', 
                                  step = 0.01, 
                                  class = 'input-box', 
                                  onchange = "setTwoNumberDecimal"),
                     )),
            tags$div(class = "stepper-fifty",
                     tags$small(h5(
                       "Estimated Lane Change Increase %"
                     )),
                     tags$div(
                       tags$input(
                         id = 'lane_change',
                         type = "number",
                         placeholder = "Lane Increase %",
                         value = "0.0",
                         min = '0',
                         max = '15',
                         step = 0.01,
                         class = 'input-box',
                         onchange = "setTwoNumberDecimal"
                       ),
                     ))
          )
        ), 
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            tags$div(tags$div(h2("Option A"), style = "width:25%;text-align:left;color:#DB9743;"),
                     tags$div(
                       tags$small(h3("COLA% Year 1")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola1_a',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       tags$small(h3("COLA% Year 2")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola2_a',
                                  type = "number",
                                  placeholder = "COLA% Year 2",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       tags$small(h3("COLA% Year 3")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola3_a',
                                  type = "number",
                                  placeholder = "COLA% Year 3",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       shinyMobile::f7DownloadButton(
                         outputId = 'down_years_a',
                         label = 'Salary Scales'
                       ), class = "line-download"
                     ),
                     style = 'display:flex;text-align:center;')
          )
        ),
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            tags$div(tags$div(h2("Option B"), style = "width:25%;text-align:left;color:#2a3a6e;"),
                     tags$div(
                       tags$small(h3("COLA% Year 1")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola1_b',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       tags$small(h3("COLA% Year 2")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola2_b',
                                  type = "number",
                                  placeholder = "COLA% Year 2",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       tags$small(h3("COLA% Year 3")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola3_b',
                                  type = "number",
                                  placeholder = "COLA% Year 3",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       shinyMobile::f7DownloadButton(
                         outputId = 'down_years_b',
                         label = 'Salary Scales'
                       ), class = "line-download"
                     ),
                     style = 'display:flex;text-align:center;')
          )
        ),
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            tags$div(tags$div(h2("Option C"), style = "width:25%;text-align:left;color:grey;"),
                     tags$div(
                       tags$small(h3("COLA% Year 1")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola1_c',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       tags$small(h3("COLA% Year 2")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola2_c',
                                  type = "number",
                                  placeholder = "COLA% Year 2",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       tags$small(h3("COLA% Year 3")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola3_c',
                                  type = "number",
                                  placeholder = "COLA% Year 3",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:25%"),
                     tags$div(
                       shinyMobile::f7DownloadButton(
                         outputId = 'down_years_c',
                         label = 'Salary Scales'
                       ), class = "line-download"
                     ),
                     style = 'display:flex;text-align:center;')
          )
        ),
        tags$div(style = "display:flex", 
                 tags$div(
                   shinyMobile::f7Button( #Triggers a renderUI that changes between plot/table
                     inputId = 'toggle',
                     label = 'Plot/Tables',
                     rounded = T,
                     color = 'black'
                   ), style = 'width:18%;padding:15px;'
                 ),
                 tags$div(
                   shinyMobile::f7DownloadButton( #various download functionalities
                     outputId = 'down_summ',
                     label = 'Summary (CSV)'
                   ), class = "other-button"
                 ),
                 tags$div(
                   shinyMobile::f7DownloadButton(
                     outputId = 'down_det',
                     label = 'Details (CSV)'
                   ), class = "other-button"
                 ),
                 tags$div(
                   shinyMobile::f7DownloadButton(
                     outputId = 'down_break',
                     label = 'Breakdown (CSV)'
                   ), class = "other-button"
                 )
        ),
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            uiOutput('table_plot') #UI switch renders between plot/table
          )
        ),
        br()
      ),
      
      # Results 5 ---------------------------------------------------------------
      shinyMobile::f7Tab( #results tab displaying plots and tables
        tabName = "5Year",
        title = "5-Year Proj.",
        icon = shinyMobile::f7Icon("chart_bar_fill"),
        #   # active = TRUE,
        tags$div(
          style = 'width:35%;',
          tags$div(
            style = 'display:flex;',
            tags$div(class = "stepper-fifty",
                     tags$small(h5(
                       "Estimated Stipend % of Total Salaries"
                     )),
                     tags$div(
                       tags$input(
                         id = 'stipends5',
                         type = "number",
                         placeholder = "Stipend %",
                         value = "0.0",
                         min = '0',
                         max = '10',
                         step = 0.01,
                         class = 'input-box',
                         onchange = "setTwoNumberDecimal"
                       ),
                     )),
            tags$div(class = "stepper-fifty",
                     tags$small(h5(
                       "Estimated Lane Change Increase %"
                     )),
                     tags$div(
                       tags$input(id = 'lane_change5', type = "number", placeholder = "Lane Increase %", value = "0.0", min = '0', max = '15', step = 0.01, class = 'input-box', onchange = "setTwoNumberDecimal"),
                     ))
          )
        ),
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            tags$div(tags$div(h2("Option A"), style = "width:16.6%;text-align:left;color:#DB9743;"),
                     tags$div(
                       tags$small(h3("COLA% Year 1")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola1_a5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 2")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola2_a5',
                                  type = "number",
                                  placeholder = "COLA% Year 2",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 3")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola3_a5',
                                  type = "number",
                                  placeholder = "COLA% Year 3",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 4")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola4_a5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 5")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola5_a5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "2",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       shinyMobile::f7DownloadButton(
                         outputId = 'down_years_a5',
                         label = 'Salary Scales'
                       ), class = "line-download"
                     ),
                     style = 'display:flex;text-align:center;')
          )
        ),
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            tags$div(tags$div(h2("Option B"), style = "width:16.6%;text-align:left;color:#2a3a6e;"),
                     tags$div(
                       tags$small(h3("COLA% Year 1")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola1_b5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 2")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola2_b5',
                                  type = "number",
                                  placeholder = "COLA% Year 2",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 3")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola3_b5',
                                  type = "number",
                                  placeholder = "COLA% Year 3",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 4")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola4_b5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 5")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola5_b5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       shinyMobile::f7DownloadButton(
                         outputId = 'down_years_b5',
                         label = 'Salary Scales'
                       ), class = "line-download"
                     ),
                     style = 'display:flex;text-align:center;')
          )
        ),
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            tags$div(tags$div(h2("Option C"), style = "width:16.6%;text-align:left;color:grey;"),
                     tags$div(
                       tags$small(h3("COLA% Year 1")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola1_c5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 2")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola2_c5',
                                  type = "number",
                                  placeholder = "COLA% Year 2",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 3")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola3_c5',
                                  type = "number",
                                  placeholder = "COLA% Year 3",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 4")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola4_c5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       tags$small(h3("COLA% Year 5")),
                       tags$div(class="stepper-input",
                                tags$input(
                                  id = 'cola5_c5',
                                  type = "number",
                                  placeholder = "COLA% Year 1",
                                  value = "3.75",
                                  min = '0',
                                  max = '10',
                                  step = 0.01,
                                  class = 'input-box',
                                  onchange = "setTwoNumberDecimal"
                                ),
                       ),
                       style = "width:16.6%"),
                     tags$div(
                       shinyMobile::f7DownloadButton(
                         outputId = 'down_years_c5',
                         label = 'Salary Scales'
                       ), class = "line-download"
                     ),
                     style = 'display:flex;text-align:center;')
          )
        ),
        tags$div(style = "display:flex",
                 tags$div(
                   shinyMobile::f7Button( #Triggers a renderUI that changes between plot/table
                     inputId = 'toggle5',
                     label = 'Plot/Tables',
                     rounded = T,
                     color = 'black'
                   ), style = 'width:18%;padding:15px;'
                 ),
                 tags$div(
                   shinyMobile::f7DownloadButton( #various download functionalities
                     outputId = 'down_summ5',
                     label = 'Summary (CSV)'
                   ), class = "other-button"
                 ),
                 tags$div(
                   shinyMobile::f7DownloadButton(
                     outputId = 'down_det5',
                     label = 'Details (CSV)'
                   ), class = "other-button"
                 ),
                 tags$div(
                   shinyMobile::f7DownloadButton(
                     outputId = 'down_break5',
                     label = 'Breakdown (CSV)'
                   ), class = "other-button"
                 )
        ),
        shinyMobile::f7Shadow(
          intensity = 16,
          hover = T,
          shinyMobile::f7Card(
            uiOutput('table_plot5') #UI switch renders between plot/table
          )
        ),
        br()
      ),
      
      # Heatmaps ----------------------------------------------------------------
      
      shinyMobile::f7Tab( 
        tabName = "Heatmaps",
        title = "Heatmaps",
        icon = shinyMobile::f7Icon("table"),
        # active = TRUE,
        shinyMobile::f7Button(
          inputId = 'down_heatmap',
          label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Heatmaps (PNG)'))
        ),
        tags$div(id = 'all_heatmaps',
                 tags$div(style = 'display:flex;', 
                          tags$div(class = 'map-table', shinyMobile::f7Shadow(
                            intensity = 16,
                            hover = T,
                            shinyMobile::f7Card(plotly::plotlyOutput('heatmap1'))
                          )),
                          tags$div(class = 'map-table', shinyMobile::f7Shadow(
                            intensity = 16,
                            hover = T,
                            shinyMobile::f7Card(plotly::plotlyOutput('heatmap2'))
                          ))
                 ),
                 tags$div(style = 'display:flex;', 
                          tags$div(class = 'map-table', shinyMobile::f7Shadow(
                            intensity = 16,
                            hover = T,
                            shinyMobile::f7Card(plotly::plotlyOutput('heatmap3'))
                          )),
                          tags$div(class = 'map-table', shinyMobile::f7Shadow(
                            intensity = 16,
                            hover = T,
                            shinyMobile::f7Card(plotly::plotlyOutput('heatmap4'))
                          )))
        )
        
      )
      
    )
  )
)

ui <- shinymanager::secure_app(ui)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  #Info credential validation 
  
  res_auth <- shinymanager::secure_server(
    check_credentials = check_creds(
      dbname = "users",
      host = "arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com",
      port = 8209,
      db_user = "admin",
      db_password = "ArxEd01742!"
    )
  )
  
  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })
  
  active_user <- reactive({
    stringr::str_remove_all(auth_output()$user, "[\r\n]") %>%
      tolower()
  })
  
  

# Description -------------------------------------------------------------

language <- reactive({
  if(stringr::str_detect(active_user() %>% tolower(), '/paras')){
    'Paraprofessional'
  } else if(stringr::str_detect(active_user() %>% tolower(), '/nurses')){
    'Nurse'
  } else if(stringr::str_detect(active_user() %>% tolower(), '/admin')){
    'Administrator'
  } else if(stringr::str_detect(active_user() %>% tolower(), '/gened')){
    'Gen Ed'
  } else if(stringr::str_detect(active_user() %>% tolower(), '/speced')){
    'Spec Ed'
  } else {
    'Teacher'
  }
})  
  
output$custom_title <- renderUI(
  tags$div(class = 'text-1',
           glue::glue("Current {language()} Salaries:"))
)
  
  
  # Log user login ----------------------------------------------------------
  
  login_time <- reactive({
    req(active_user())
    query <- glue::glue("UPDATE userlist SET last_login = '{time}' WHERE user = '{user}';
                         ", 
                        user = active_user(), 
                        time = strftime(as.POSIXlt(Sys.time(), tz = "EST"), '%F %T', usetz = TRUE))
    sqlQuery(query, 'users')
  })
  
  
  # Change password logic ---------------------------------------------------
  
  observeEvent(input$pass, { #Observe for submit button. If passwords match, update on server. If not, notify user
    shinyMobile::f7Popup(
      id = "popup1",
      title = tags$div(class = 'pass-title', h2("Change Password")),
      tags$div(
        shinyMobile::f7Text("newPass", "New Password", ""),
        shinyMobile::f7Text("newPassConf", "Confirm New Password", ""),
        br(),
        shinyMobile::f7Button('submit_pass', 'Submit', size = 'large')
      ),
    )
  })
  
  observeEvent(input$submit_pass, {
    if(input$newPassConf == input$newPass){
      
      query <- glue::glue("UPDATE userlist
                               SET password = '{password}'
                               WHERE user = '{user}';
                               ", user = active_user(), password = input$newPass)
      sqlQuery(query, 'users')
      
      shinyMobile::f7Notif(
        text = "Your password has succesfully changed",
        icon = shinyMobile::f7Icon("checkmark_alt_circle"),
        title = "Password Updated",
        titleRightText = "now",
        closeTimeout = 5500,
        closeButton = F
      )
      
    }else {
      shinyMobile::f7Notif(
        text = "Passwords don't match. Please try again",
        icon = shinyMobile::f7Icon("xmark_circle"),
        title = "Update Error",
        titleRightText = "now",
        closeTimeout = 5500,
        closeButton = T
      )
    }
    
  })
  
  
  #get associated district logo and name
  
  logo <- reactive({ #access district table to get name and logo associated with user
    query <- paste0("SELECT logo
                      FROM district_user
                      WHERE user =", "'",  active_user(), "';") 
    
    sqlQuery(query, 'districts')%>%
      unique() %>% 
      as.character()
  })
  
  district_name <- reactive({
    
    query <- paste0("SELECT district_name 
                      FROM district_user 
                      WHERE user =", "'",  active_user(),"';")
    sqlQuery(query,'districts') %>% 
      unique()
    
  })
  
  output$logo_src <- renderUI(
    tags$div(
      style = 'display:flex',
      tags$div(
        class = 'dis-name',
        district_name()
      ),
      tags$div(
        tags$img(src = logo(), height = '50px')
      )
    )
    
  )
  
  
  #Creating the template
  
  temp_data <- reactive({data.frame(matrix('0', ncol = as.numeric(input$lanes)+1, nrow = as.numeric(input$steps))) %>%
      dplyr::mutate(X1 = seq_len(as.numeric(input$steps))) %>%
      dplyr::rename(Step = X1) %>% 
      dplyr::rename_with(spec_rename, dplyr::starts_with('X'))
  })
  
  #Download handlers for templates
  
  output$sal_template = downloadHandler(
    filename = function() {
      paste("Salary Table Template ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(temp_data(), file, row.names = F)
    }
  )
  
  output$fte_template = downloadHandler(
    filename = function() {
      paste("FTE Table Template ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(temp_data(), file, row.names = F)
    }
  )
  
  
  salary_in <- reactive({
    input$sal
  })
  
  fte_in <- reactive({
    input$fte
  })
  
  #Try to get info from database
  
  test_server <- reactive({
    req(active_user())
    query <- glue::glue("SELECT * 
                    FROM user_scales 
                    WHERE user = '{active}';",
                    active = active_user())
    
    dim(sqlQuery(query, 'salary'))
  })
  
  
  #Get info from database or uploaded files if no uploaded data 
  
  salary_upload <- reactive({
    if (!purrr::is_empty(salary_in()[1])) {
      readr::read_csv(salary_in()[['datapath']]) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(),
                                    function(x)
                                      readr::parse_number(as.character(x))))
    }
  })
  
  fte_upload <- reactive({
    if (!purrr::is_empty(fte_in()[1])) {
      readr::read_csv(fte_in()[['datapath']]) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(),
                                    function(x)
                                      readr::parse_number(as.character(x))))
    }
  })
  
  
  uploaded_scales <- reactive({
    expand_grids(salary_upload(), fte_upload(), active_user())
  })

  
  active_scales <- reactive({
    if(!purrr::is_empty(fte_in()[1]) & !purrr::is_empty(salary_in()[1])){
      uploaded_scales()
    } else if(!purrr::is_empty(test_server()[1] != 0)){
      query <- glue::glue("SELECT * 
                    FROM user_scales 
                    WHERE user = '{active}';",
                          active = active_user())
      
      sqlQuery(query, 'salary')%>%
        tibble::tibble()
    }
  })
  
  
  #Process the table data to get yearly payroll (raw number and formatted)

  payroll_raw <- reactive({
    req(active_scales())
    temp <- active_scales() %>%
      dplyr::mutate(pay = salary*fte)
    sum(temp$pay)

  })

  payroll <- reactive({
    req(active_scales())
    paste0('$', formatC(payroll_raw(),  format="f", digits=0, big.mark=","))

  })
  
  output$payroll <- renderText(
    payroll()
  )

  total_fte <- reactive({
    req(active_scales())
    sum(active_scales()$fte)

  })

  output$employees <- renderText(
    total_fte()
  )

  bottom_step <- reactive({ #calculate total fte
    req(active_scales())
    temp <- active_scales() %>%
      dplyr::filter(salary != 0 & !is.na(salary)) %>%
      dplyr::group_by(col_num) %>%
      dplyr::filter(row_num == max(row_num)) %>%
      dplyr::ungroup()

    sum(temp$fte)
  })

  
  output$last_step <- renderText(
    paste0(round((bottom_step()/total_fte())*100, 1),'%')
  )

  # Notifies the user that save succesful (not real) and directs
  observeEvent(input$save,{ 
    shinyMobile::f7Notif(
      text = "Your data is being saved",
      icon = shinyMobile::f7Icon("rays"),
      title = "Please Wait",
      titleRightText = "now",
      closeTimeout = 10000,
      closeButton = T
    )
  })

  observeEvent(input$save, {
    table <- active_scales() %>% 
      data.frame()
    values <- glue::glue('("{col1}", "{col2}", "{col3}", "{col4}", "{col5}")',
                         col1 = table[1, 1],
                         col2 = table[1, 2],
                         col3 = table[1, 3],
                         col4 = table[1, 4],
                         col5 = table[1, 5])
    
    #Append all value lists to a query
    for(i in 2:nrow(active_scales())){
      values <- glue::glue(values,
                           ', ("{col1}", "{col2}", "{col3}", "{col4}", "{col5}")',
                           col1 = table[i, 1],
                           col2 = table[i, 2],
                           col3 = table[i, 3],
                           col4 = table[i, 4],
                           col5 = table[i, 5]
      )
    }
    
    query <- glue::glue("DELETE FROM user_scales where user = '{active}';",
                        active = active_user())
    sqlQuery(query, 'salary')

    query <- glue::glue('INSERT INTO user_scales VALUES', values, ';')
    sqlQuery(query, 'salary')
  })
  


  # 
  observeEvent(input$save,{ # Notifies the user that save succesful (not real) and directs
    shinyMobile::f7Notif(
      text = "Go to Results page",
      icon = shinyMobile::f7Icon("cloud_upload"),
      title = "Save Successful",
      titleRightText = "now",
      closeTimeout = 5500,
      closeButton = T
    )
  })


  # Results Tab --------------------------------------------------------------

  # Get progresed FTE scales
  year_1 <- reactive({ 
    req(active_scales())
    year_progress(active_scales())
  })
  
  year_2 <- reactive({ 
    year_progress(year_1())
  })
  
  year_3 <- reactive({ 
    year_progress(year_2())
  })
  
  
  # Convert numbers to correct % (stipend and COLA)
  stipends <- reactive({
    (input$stipends/100)
  })

  lane_change <- reactive({
    (input$lane_change/100)
  })

  cola_a1 <- reactive({
    1+(input$cola1_a/100)
  })

  cola_a2 <- reactive({
    1+(input$cola2_a/100)
  })

  cola_a3 <- reactive({
    1+(input$cola3_a/100)
  })

  cola_b1 <- reactive({
    1+(input$cola1_b/100)
  })

  cola_b2 <- reactive({
    1+(input$cola2_b/100)
  })

  cola_b3 <- reactive({
    1+(input$cola3_b/100)
  })

  cola_c1 <- reactive({
    1+(input$cola1_c/100)
  })

  cola_c2 <- reactive({
    1+(input$cola2_c/100)
  })

  cola_c3 <- reactive({
    1+(input$cola3_c/100)
  })
  
  
  # adjust year by year salary with COLA% (all options)
  
  year_increase <- function(scales, cola, stipends = 0, lane_change = 0){
    scales %>% 
      dplyr::mutate(salary = salary*cola*(1+stipends+lane_change))
  }
  
  get_total_salary <- function(scales){
    scales %>% 
      dplyr::mutate(pay = salary*fte) %>% 
      dplyr::summarise(n = sum(pay, na.rm = T)) %>% 
      dplyr::pull(n)
  }
  
  # Calculate the new advanced scales with financial impacts
  scales_a_year1 <- reactive({
    year_increase(year_1(), cola_a1(), stipends(), lane_change()) 
  })
  
  # Extract the salary
  year_1_total_a <- reactive({
    get_total_salary(scales_a_year1())
  })
  
  scales_a_year2 <- reactive({
    year_increase(year_progress(scales_a_year1()), cola_a2(), stipends(), lane_change()) 
  })
  
  year_2_total_a <- reactive({
    get_total_salary(scales_a_year2())
  })
  
  scales_a_year3 <- reactive({
    year_increase(year_progress(scales_a_year2()), cola_a3(), stipends(), lane_change()) 
  })
  
  year_3_total_a <- reactive({
    get_total_salary(scales_a_year3())
  })

  scales_b_year1 <- reactive({
    year_increase(year_1(), cola_b1(), stipends(), lane_change()) 
  })
  
  year_1_total_b <- reactive({
    get_total_salary(scales_b_year1())
  })
  
  scales_b_year2 <- reactive({
    year_increase(year_progress(scales_b_year1()), cola_b2(), stipends(), lane_change()) 
  })
  
  year_2_total_b <- reactive({
    get_total_salary(scales_b_year2())
  })
  
  scales_b_year3 <- reactive({
    year_increase(year_progress(scales_b_year2()), cola_b3(), stipends(), lane_change()) 
  })
  
  year_3_total_b <- reactive({
    get_total_salary(scales_b_year3())
  })  
  
  scales_c_year1 <- reactive({
    year_increase(year_1(), cola_c1(), stipends(), lane_change()) 
  })
  
  year_1_total_c <- reactive({
    get_total_salary(scales_c_year1())
  })
  
  scales_c_year2 <- reactive({
    year_increase(year_progress(scales_c_year1()), cola_c2(), stipends(), lane_change()) 
  })
  
  year_2_total_c <- reactive({
    get_total_salary(scales_c_year2())
  })
  
  scales_c_year3 <- reactive({
    year_increase(year_progress(scales_c_year2()), cola_c3(), stipends(), lane_change()) 
  })
  
  year_3_total_c <- reactive({
    get_total_salary(scales_c_year3())
  })
  
  plot_data <- reactive({
    tibble::tibble(
      op_a = c(year_1_total_a(),
               year_2_total_a(),
               year_3_total_a()),
      op_b = c(year_1_total_b(),
               year_2_total_b(),
               year_3_total_b()),
      op_c = c(year_1_total_c(),
               year_2_total_c(),
               year_3_total_c())
    )
  })

  # #Render plot with plotly

  output$budget_plot <- plotly::renderPlotly({
    plotly::plot_ly(
      plot_data(),
      x = ~ c('Year 1', 'Year 2', 'Year 3'),
      y = ~ op_a,
      type = 'bar',
      name = 'Option A',
      marker = list(color = '#DB9743'),
      hovertemplate = 'Salaries: %{y:$,.0f}<extra></extra>',
      texttemplate = '$%{y:.3s}',
      textposition = 'inside',
      height = 500
    ) %>%
      plotly::add_trace(y = ~ op_b,
                        name = 'Option B',
                        marker = list(color = '#2a3a6e')) %>%
      plotly::add_trace(y = ~ op_c,
                        name = 'Option C',
                        marker = list(color = 'grey')) %>%
      plotly::layout(
        xaxis = list(title = "", tickangle = -45, fixedrange = T),
        yaxis = list(title = "", range = c(max(plot_data())*0.66, max(plot_data()*1.05)), fixedrange = T),
        margin = list(b = 100),
        barmode = 'group',
        showlegend =F,
        font = list(size = 18),
        images = list(
          source = "https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png",
          x = -0.03, y = -0.1,
          sizex = 0.20, sizey = 0.10
        )
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      ) %>%
      plotly::add_annotations(
        text= glue::glue('Current {language()} Salaries: {payroll()}'),
        align='left',
        showarrow= F,
        xref='paper',
        yref='paper',
        x=0.02,
        y=0.99,
        bordercolor='black',
        borderwidth=1
      )
  })

  # #creating various tables for download (csv and png with {gt})

  detail_table <- reactive({
    tibble::tibble(
      option= c('Option A', 'Option B', 'Option C'),
      cola_year_1 = c(
        input$cola1_a,
        input$cola1_b,
        input$cola1_c
      ),
      salaries_year_1 = c(
        round(year_1_total_a()),
        round(year_1_total_b()),
        round(year_1_total_c())
      ),
      perc_increase_1  =c(
        round((((year_1_total_a())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_1_total_b())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_1_total_c())-payroll_raw())*100)/payroll_raw(),2 )
      ),
      cola_year_2 = c(
        input$cola2_a,
        input$cola2_b,
        input$cola2_c
      ),
      salaries_year_2 = c(
        round(year_2_total_a()),
        round(year_2_total_b()),
        round(year_2_total_c())
      ),
      perc_increase_2  =c(
        round((((year_2_total_a())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_2_total_b())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_2_total_c())-payroll_raw())*100)/payroll_raw(),2 )
      ),
      cola_year_3 = c(
        input$cola3_a,
        input$cola3_b,
        input$cola3_c
      ),
      salaries_year_3 = c(
        round(year_3_total_a()),
        round(year_3_total_b()),
        round(year_3_total_c())
      ),
      perc_increase_3  =c(
        round(((year_3_total_a()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_3_total_b()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_3_total_c()-payroll_raw())*100)/payroll_raw(),2 )
      )
    )
  })

  table_data <- reactive({
    tibble::tibble(

      option = c('Option A', 'Option B', 'Option C'),

      total_salaries = c(
        round(year_3_total_a()),
        round(year_3_total_b()),
        round(year_3_total_c())
      ),
      total_increase = c(
        round(year_3_total_a() - payroll_raw()),
        round(year_3_total_b() - payroll_raw()),
        round(year_3_total_c() - payroll_raw())
      ),
      perc_increase = c(
        round(((year_3_total_a()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_3_total_b()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_3_total_c()-payroll_raw())*100)/payroll_raw(),2 )
      )
    )
  })
  

  cola_increase_a <- reactive({
    year_3_total_a() - # Final Total Amount
      round(sum(scales_a_year3()$fte*active_scales()$salary, na.rm = T) + # Effect from FTE
              # Effect Stipends on 1
              get_total_salary(scales_a_year1()) - sum(scales_a_year1()$fte*active_scales()$salary*cola_a1(), na.rm = T) +  
              # Effect Stipends on 2
              get_total_salary(scales_a_year2()) - sum(scales_a_year2()$fte*scales_a_year1()$salary*cola_a2(), na.rm = T) + 
              # Effect Stipends on 3
              get_total_salary(scales_a_year3()) - sum(scales_a_year3()$fte*scales_a_year2()$salary*cola_a3(), na.rm = T))  
  })
  
  cola_increase_b <- reactive({
    year_3_total_b() -
      round(sum(scales_b_year3()$fte*active_scales()$salary, na.rm = T) +
              get_total_salary(scales_b_year1()) - sum(scales_b_year1()$fte*active_scales()$salary*cola_b1(), na.rm = T) +
              get_total_salary(scales_b_year2()) - sum(scales_b_year2()$fte*scales_b_year1()$salary*cola_b2(), na.rm = T) +
              get_total_salary(scales_b_year3()) - sum(scales_b_year3()$fte*scales_b_year2()$salary*cola_b3(), na.rm = T))
  })
  
  cola_increase_c <- reactive({
    year_3_total_c() -
      round(sum(scales_c_year3()$fte*active_scales()$salary, na.rm = T) +
              get_total_salary(scales_c_year1()) - sum(scales_c_year1()$fte*active_scales()$salary*cola_c1(), na.rm = T) +
              get_total_salary(scales_c_year2()) - sum(scales_c_year2()$fte*scales_c_year1()$salary*cola_c2(), na.rm = T) +
              get_total_salary(scales_c_year3()) - sum(scales_c_year3()$fte*scales_c_year2()$salary*cola_c3(), na.rm = T))
  })
  
  breakdown_table <- reactive({
    tibble::tibble(
      option = c('Option A', 'Option B', 'Option C'),
      
      total_increase = c(
        round(year_3_total_a() - payroll_raw()),
        round(year_3_total_b() - payroll_raw()),
        round(year_3_total_c() - payroll_raw())
      ),
      cola_increase_total = c(
        round(cola_increase_a()),
        round(cola_increase_b()),
        round(cola_increase_c())

      ),
      increase_from_cola = c(
        round(100*cola_increase_a()/(year_3_total_a() - payroll_raw()),2),
        round(100*cola_increase_b()/(year_3_total_b() - payroll_raw()),2),
        round(100*cola_increase_c()/(year_3_total_c() - payroll_raw()),2)
      ),
      step_increase_total = c(
        round(sum(scales_a_year3()$fte*active_scales()$salary, na.rm = T) - payroll_raw()),
        round(sum(scales_b_year3()$fte*active_scales()$salary, na.rm = T) - payroll_raw()),
        round(sum(scales_c_year3()$fte*active_scales()$salary, na.rm = T) - payroll_raw())
        
      ),
      increase_from_steps = c(
        round(100*(sum(scales_a_year3()$fte*active_scales()$salary, na.rm = T) - payroll_raw())/
          (year_3_total_a() - payroll_raw()),2),
        round(100*(sum(scales_b_year3()$fte*active_scales()$salary, na.rm = T) - payroll_raw())/
          (year_3_total_b() - payroll_raw()),2),
        round(100*(sum(scales_c_year3()$fte*active_scales()$salary, na.rm = T) - payroll_raw())/
          (year_3_total_c() - payroll_raw()),2)
        
      ),
      other_total = c(
        (year_1_total_a()- 
          sum(scales_a_year1()$fte*active_scales()$salary*cola_a1(), na.rm = T) + 
          year_2_total_a()- sum(scales_a_year2()$fte*scales_a_year1()$salary*cola_a2(), na.rm = T) + 
          year_3_total_a()- sum(scales_a_year3()$fte*scales_a_year2()$salary*cola_a3(), na.rm = T) )%>% 
          round(),
        (year_1_total_b()- 
          sum(scales_b_year1()$fte*active_scales()$salary*cola_b1(), na.rm = T) + 
          year_2_total_b()- sum(scales_b_year2()$fte*scales_b_year1()$salary*cola_b2(), na.rm = T) + 
          year_3_total_b()- sum(scales_b_year3()$fte*scales_b_year2()$salary*cola_b3(), na.rm = T) )%>% 
          round(),
        (year_1_total_c()- 
          sum(scales_c_year1()$fte*active_scales()$salary*cola_c1(), na.rm = T) + 
          year_2_total_c()- sum(scales_c_year2()$fte*scales_c_year1()$salary*cola_c2(), na.rm = T) + 
          year_3_total_c()- sum(scales_c_year3()$fte*scales_c_year2()$salary*cola_c3(), na.rm = T) )%>% 
          round()
        
      ),
      others = c(
        (100*(year_1_total_a()- 
          sum(scales_a_year1()$fte*active_scales()$salary*cola_a1(), na.rm = T) + 
          year_2_total_a()- sum(scales_a_year2()$fte*scales_a_year1()$salary*cola_a2(), na.rm = T) + 
          year_3_total_a()- sum(scales_a_year3()$fte*scales_a_year2()$salary*cola_a3(), na.rm = T))/
          (year_3_total_a() - payroll_raw()) )%>% 
          round(2),
        (100*(year_1_total_b()- 
          sum(scales_b_year1()$fte*active_scales()$salary*cola_b1(), na.rm = T) + 
          year_2_total_b()- sum(scales_b_year2()$fte*scales_b_year1()$salary*cola_b2(), na.rm = T) + 
          year_3_total_b()- sum(scales_b_year3()$fte*scales_b_year2()$salary*cola_b3(), na.rm = T))/
          (year_3_total_b() - payroll_raw())) %>% 
          round(2),
        (100*(year_1_total_c()- 
          sum(scales_c_year1()$fte*active_scales()$salary*cola_c1(), na.rm = T) + 
          year_2_total_c()- sum(scales_c_year2()$fte*scales_c_year1()$salary*cola_c2(), na.rm = T) + 
          year_3_total_c()- sum(scales_c_year3()$fte*scales_c_year2()$salary*cola_c3(), na.rm = T))/
          (year_3_total_c() - payroll_raw())) %>% 
          round()
        )
      )
    })


  years_table_a <- reactive({
      dplyr::bind_cols('Cola 1' = input$cola1_a,
                       reshape2::acast(scales_a_year1(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                       value.var = 'salary', fill = '0') %>% 
                         tibble::as_tibble() %>% 
                         dplyr::rename_with(function(x)paste('Year 1 - Lane', x)),
                       'Cola 2' = input$cola2_a,
                       reshape2::acast(scales_a_year2(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                       value.var = 'salary', fill = '0') %>% 
                         tibble::as_tibble() %>% 
                         dplyr::rename_with(function(x)paste('Year 2 - Lane', x)),
                       'Cola 3' = input$cola3_a,
                       reshape2::acast(scales_a_year3(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                       value.var = 'salary', fill = '0') %>% 
                         tibble::as_tibble() %>% 
                         dplyr::rename_with(function(x)paste('Year 3 - Lane', x))
      ) %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with('Year'), function(x){round(as.numeric(x))}))

  })
  
  years_table_b <- reactive({
    dplyr::bind_cols('Cola 1' = input$cola1_b,
                     reshape2::acast(scales_b_year1(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 1 - Lane', x)),
                     'Cola 2' = input$cola2_b,
                     reshape2::acast(scales_b_year2(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 2 - Lane', x)),
                     'Cola 3' = input$cola3_b,
                     reshape2::acast(scales_b_year3(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x))
    ) %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with('Year'), function(x){round(as.numeric(x))}))
    
  })
  
  years_table_c <- reactive({
    dplyr::bind_cols('Cola 1' = input$cola1_c,
                     reshape2::acast(scales_c_year1(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 1 - Lane', x)),
                     'Cola 2' = input$cola2_c,
                     reshape2::acast(scales_c_year2(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 2 - Lane', x)),
                     'Cola 3' = input$cola3_c,
                     reshape2::acast(scales_c_year3(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x))
    ) %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with('Year'), function(x){round(as.numeric(x))}))
    
  })


  #download handlers for all csv and png downloads

  output$down_summ = downloadHandler(
    filename = function() {
      paste("Salary Summary ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_data(), file, row.names = F)
    }
  )

  output$down_det = downloadHandler(
    filename = function() {
      paste("Salary Details ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(detail_table(), file, row.names = F)
    }
  )

  output$down_break = downloadHandler(
    filename = function() {
      paste("Salary Breakdown ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(breakdown_table(), file, row.names = F)
    }
  )

  output$down_years_a = downloadHandler(
    filename = function() {
      paste("Salary Scales (A) ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(years_table_a(), file, row.names = F)
    }
  )

  output$down_years_b = downloadHandler(
    filename = function() {
      paste("Salary Scales (B) ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(years_table_b(), file, row.names = F)
    }
  )

  output$down_years_c = downloadHandler(
    filename = function() {
      paste("Salary Scales (C) ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(years_table_c(), file, row.names = F)
    }
  )

  # #displayed gt tables

  summary_tab <- reactive({
    table_data() %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::md("**Three-Year Projections**"),
        subtitle = gt::html(paste0(tags$span(style = "font-size:15px;text-align:left;" , 'Current', language(), 'Salaries: ', payroll())))
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#DB9743')
        ),
        locations = gt::cells_body(rows = 1)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#2a3a6e')
        ),
        locations = gt::cells_body(rows = 2)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#9399a3')
        ),
        locations = gt::cells_body(rows = 3)
      ) %>%
      gt::fmt_number(columns = c(total_salaries, total_increase),
                     use_seps  = T,
                     sep_mark = ',',
                     pattern = '${x}',
                     decimals = 0
      ) %>%
      gt::fmt_number(columns = c(perc_increase),
                     decimals = 1,
                     patter = '{x}%') %>%
      gt::tab_source_note(
        source_note = gt::html(paste0(tags$span(style = "font-size:17px;" , tags$span(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png',width='50px', style = 'margin-right:3px;margin-bottom:-3px;')), "LITIX", 'Budget Forecasting')))
      ) %>%
      # gt::tab_source_note(
      #     source_note = gt::html(paste0(shinyMobile::f7Link(href = 'https://www.arxed.com/', label = 'Visit Our Website')))
      #     ) %>%
      gt::cols_width(
        dplyr::everything() ~ gt::pct(20)
      ) %>%
      gt::cols_label(
        total_salaries = "Total Salaries",
        total_increase = "3-yr Salary Increase",
        perc_increase = "3-yr % Salary Increase",
        option = 'Proj. Option'
      ) %>%
      # gt::opt_table_font(
      # font = list(
      # gt::google_font("Roboto Mono"),
      #          gt::default_fonts()),
      # weight = 400) %>%
      gt::tab_options(column_labels.font.size = '100%',
                      column_labels.font.weight = '600') %>%
      gt::cols_align(align = c("center"),
                     columns = c(option:perc_increase)) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_title("subtitle")
      )
  })

  details_tab <- reactive({
    detail_table() %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::md("**Three-Year Projections (Detailed)**"),
        subtitle = gt::html(paste0(tags$span(style = "font-size:15px;text-align:left;" , 'Current', language(), 'Salaries: ', payroll())))
      ) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_title("subtitle")
      ) %>%
      gt::tab_spanner(
        label = "Year 1",
        columns = c(cola_year_1, salaries_year_1, perc_increase_1)
      ) %>%
      gt::tab_spanner(
        label = "Year 2",
        columns = c(cola_year_2, salaries_year_2, perc_increase_2)
      ) %>%
      gt::tab_spanner(
        label = "Year 3",
        columns = c(cola_year_3, salaries_year_3, perc_increase_3)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#DB9743')
        ),
        locations = gt::cells_body(rows = 1)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#2a3a6e')
        ),
        locations = gt::cells_body(rows = 2)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#9399a3')
        ),
        locations = gt::cells_body(rows = 3)
      ) %>%
      gt::fmt_number(columns = c(salaries_year_1, salaries_year_2,salaries_year_3),
                     use_seps  = T,
                     sep_mark = ',',
                     pattern = '${x}',
                     decimals = 0
      ) %>%
      gt::fmt_number(columns = c(perc_increase_1,perc_increase_2,perc_increase_3),
                     decimals = 1,
                     patter = '{x}%') %>%
      gt::fmt_number(columns = c(cola_year_1,cola_year_2,cola_year_3),
                     decimals = 2,
                     patter = '{x}%') %>%
      gt::tab_source_note(
        source_note = gt::html(paste0(tags$span(style = "font-size:17px;" , tags$span(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png',width='50px', style = 'margin-right:3px;margin-bottom:-3px;')), "LITIX", 'Budget Forecasting')))
      ) %>%
      gt::cols_label(
        cola_year_1 = "COLA",
        salaries_year_1 = "Salary",
        perc_increase_1 = "% Increase",
        cola_year_2 = "COLA",
        salaries_year_2 = "Salary",
        perc_increase_2 = "% Increase",
        cola_year_3 = "COLA",
        salaries_year_3 = "Salary",
        perc_increase_3 = "% Increase",
        # total_salary = 'Total 3-Yr Salary',
        option = 'Proj. Option'
      ) %>%
      gt::cols_width(
        dplyr::everything() ~ gt::pct(10)
      ) %>%
      # gt::opt_table_font(
      #    font = list(gt::google_font("Roboto Mono"),
      #                gt::default_fonts()),
      #                weight = 400) %>%
      gt::tab_options(column_labels.font.size = '100%',
                      column_labels.font.weight = '600') %>%
      gt::cols_align(align = c("center"),
                     columns = c(option:perc_increase_3))
  })

  breakdown_tab <- reactive({
    breakdown_table() %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::md("**Three-Year Increase Breakdown**"),
        subtitle = gt::html(paste0(tags$span(style = "font-size:15px;text-align:left;" , 'Current', language(), 'Salaries: ', payroll())))
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#DB9743')
        ),
        locations = gt::cells_body(rows = 1)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#2a3a6e')
        ),
        locations = gt::cells_body(rows = 2)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#9399a3')
        ),
        locations = gt::cells_body(rows = 3)
      ) %>%
      gt::fmt_number(columns = c(total_increase, cola_increase_total, step_increase_total, other_total),
                     use_seps  = T,
                     sep_mark = ',',
                     pattern = '${x}',
                     decimals = 0
      ) %>%
      gt::fmt_number(columns = c(increase_from_cola, increase_from_steps, others),
                     decimals = 1,
                     patter = '{x}%') %>%
      gt::tab_source_note(
        source_note = gt::html(paste0(tags$span(style = "font-size:17px;" , tags$span(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png',width='50px', style = 'margin-right:3px;margin-bottom:-3px;')), "LITIX", 'Budget Forecasting')))
      ) %>%
      gt::cols_width(
        dplyr::everything() ~ gt::pct(20)
      ) %>%
      gt::cols_label(
        total_increase = "Total 3-Year Increase",
        cola_increase_total = "Total Increase from COLA",
        increase_from_cola = "% of Increase from COLA",
        step_increase_total = "Total Increase from Step Progression",
        increase_from_steps = "% of Increase from Steps",
        other_total = 'Others (Total)',
        others = 'Others',
        option = 'Proj. Option'
      ) %>%
      gt::tab_options(column_labels.font.size = '100%',
                      column_labels.font.weight = '600') %>%
      gt::cols_align(align = c("center"),
                     columns = c(option:others)) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_title("subtitle")
      )
  })

  output$sum_table <- gt::render_gt({
    summary_tab()
  })

  output$det_table <- gt::render_gt({
    details_tab()
  })

  output$break_table <- gt::render_gt({
    breakdown_tab()
  })

  #Conditional UI that switches between plot and tables

  output$table_plot <- renderUI({
    if(input$toggle %% 2 == 0){
      tags$div(
        plotly::plotlyOutput('budget_plot', height = 'auto'),
        tags$div(
          shinyMobile::f7Button(
            inputId = 'down_chart',
            label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Bar Chart')) #fas fa classes corresponf to fontawesome
          ),
          class = "other-button card-button2"))
    } else if(input$toggle %% 2 == 1){
      tags$div(tags$div(
        tags$div(shinyMobile::f7Button(
          inputId = 'down_summ_img',
          label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Summary (PNG)'))
        ),
        class = "other-button card-button"),
        gt::gt_output('sum_table')),
        tags$div(
          tags$div(shinyMobile::f7Button(
            inputId = 'down_det_img',
            label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Details (PNG)'))
          ),
          class = "other-button card-button"),
          gt::gt_output('det_table')),
        tags$div(
          tags$div(shinyMobile::f7Button(
            inputId = 'down_break_img',
            label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Breakdown (PNG)'))
          ),
          class = "other-button card-button"),
          gt::gt_output('break_table')))
    } else{
    }
  })


  observeEvent(input$down_chart, { #take "screenshot" of plot selector
    shinyscreenshot::screenshot(selector = '#budget_plot' , filename = 'Salary Chart.png', scale = 3)
  })

  observeEvent(input$down_summ_img, { #take "screenshot" of table selector
    shinyscreenshot::screenshot(selector = '#sum_table' , filename = 'Summary Table', scale = 3)
  })

  observeEvent(input$down_det_img, {
    shinyscreenshot::screenshot(selector = '#det_table' , filename = 'Details Table', scale = 3)
  })

  observeEvent(input$down_break_img, {
    shinyscreenshot::screenshot(selector = '#break_table' , filename = 'Breakdown Table', scale = 3)
  })

  # Results 5 ---------------------------------------------------------------
  
  # Get progresed FTE scales
  year_4 <- reactive({ 
    year_progress(year_3())
  })
  
  year_5 <- reactive({ 
    year_progress(year_4())
  })
  
  
  
  # Convert numbers to correct % (stipend and COLA)
  stipends5 <- reactive({
    (input$stipends5/100)
  })
  
  lane_change5 <- reactive({
    (input$lane_change5/100)
  })
  
  cola_a1_5 <- reactive({
    1+(input$cola1_a5/100)
  })
  
  cola_a2_5 <- reactive({
    1+(input$cola2_a5/100)
  })
  
  cola_a3_5 <- reactive({
    1+(input$cola3_a5/100)
  })
  
  cola_a4_5 <- reactive({
    1+(input$cola4_a5/100)
  })
  
  cola_a5_5 <- reactive({
    1+(input$cola5_a5/100)
  })
  
  cola_b1_5 <- reactive({
    1+(input$cola1_b5/100)
  })
  
  cola_b2_5 <- reactive({
    1+(input$cola2_b5/100)
  })
  
  cola_b3_5 <- reactive({
    1+(input$cola3_b5/100)
  })
  
  cola_b4_5 <- reactive({
    1+(input$cola4_b5/100)
  })
  
  cola_b5_5 <- reactive({
    1+(input$cola5_b5/100)
  })
  
  cola_c1_5 <- reactive({
    1+(input$cola1_c5/100)
  })
  
  cola_c2_5 <- reactive({
    1+(input$cola2_c5/100)
  })
  
  cola_c3_5 <- reactive({
    1+(input$cola3_c5/100)
  })
  
  cola_c4_5 <- reactive({
    1+(input$cola4_c5/100)
  })
  
  cola_c5_5 <- reactive({
    1+(input$cola5_c5/100)
  })
  
  # Calculate the new advanced scales with financial impacts
  scales_a_year1_5 <- reactive({
    year_increase(year_1(), cola_a1_5(), stipends5(), lane_change5()) 
  })
  
  # Extract the salary
  year_1_total_a5 <- reactive({
    get_total_salary(scales_a_year1_5())
  })
  
  scales_a_year2_5 <- reactive({
    year_increase(year_progress(scales_a_year1_5()), cola_a2_5(), stipends(), lane_change()) 
  })
  
  year_2_total_a5 <- reactive({
    get_total_salary(scales_a_year2())
  })
  
  scales_a_year3_5 <- reactive({
    year_increase(year_progress(scales_a_year2_5()), cola_a3_5(), stipends5(), lane_change5()) 
  })
  
  year_3_total_a5 <- reactive({
    get_total_salary(scales_a_year3_5())
  })
  
  scales_a_year4_5 <- reactive({
    year_increase(year_progress(scales_a_year3_5()), cola_a4_5(), stipends5(), lane_change5()) 
  })
  
  year_4_total_a5 <- reactive({
    get_total_salary(scales_a_year4_5())
  })
  
  scales_a_year5_5 <- reactive({
    year_increase(year_progress(scales_a_year4_5()), cola_a3_5(), stipends5(), lane_change5()) 
  })
  
  year_5_total_a5 <- reactive({
    get_total_salary(scales_a_year5_5())
  })
  
  
  # Calculate the new advanced scales with financial impacts
  scales_b_year1_5 <- reactive({
    year_increase(year_1(), cola_b1_5(), stipends5(), lane_change5()) 
  })
  
  # Extract the salary
  year_1_total_b5 <- reactive({
    get_total_salary(scales_b_year1_5())
  })
  
  scales_b_year2_5 <- reactive({
    year_increase(year_progress(scales_b_year1_5()), cola_b2_5(), stipends(), lane_change()) 
  })
  
  year_2_total_b5 <- reactive({
    get_total_salary(scales_b_year2())
  })
  
  scales_b_year3_5 <- reactive({
    year_increase(year_progress(scales_b_year2_5()), cola_b3_5(), stipends5(), lane_change5()) 
  })
  
  year_3_total_b5 <- reactive({
    get_total_salary(scales_b_year3_5())
  })
  
  scales_b_year4_5 <- reactive({
    year_increase(year_progress(scales_b_year3_5()), cola_b4_5(), stipends5(), lane_change5()) 
  })
  
  year_4_total_b5 <- reactive({
    get_total_salary(scales_b_year4_5())
  })
  
  scales_b_year5_5 <- reactive({
    year_increase(year_progress(scales_b_year4_5()), cola_b3_5(), stipends5(), lane_change5()) 
  })
  
  year_5_total_b5 <- reactive({
    get_total_salary(scales_b_year5_5())
  })
  
  
  # Calculate the new advanced scales with financial impacts
  scales_c_year1_5 <- reactive({
    year_increase(year_1(), cola_c1_5(), stipends5(), lane_change5()) 
  })
  
  # Extract the salary
  year_1_total_c5 <- reactive({
    get_total_salary(scales_c_year1_5())
  })
  
  scales_c_year2_5 <- reactive({
    year_increase(year_progress(scales_c_year1_5()), cola_c2_5(), stipends(), lane_change()) 
  })
  
  year_2_total_c5 <- reactive({
    get_total_salary(scales_c_year2())
  })
  
  scales_c_year3_5 <- reactive({
    year_increase(year_progress(scales_c_year2_5()), cola_c3_5(), stipends5(), lane_change5()) 
  })
  
  year_3_total_c5 <- reactive({
    get_total_salary(scales_c_year3_5())
  })
  
  scales_c_year4_5 <- reactive({
    year_increase(year_progress(scales_c_year3_5()), cola_c4_5(), stipends5(), lane_change5()) 
  })
  
  year_4_total_c5 <- reactive({
    get_total_salary(scales_c_year4_5())
  })
  
  scales_c_year5_5 <- reactive({
    year_increase(year_progress(scales_c_year4_5()), cola_c3_5(), stipends5(), lane_change5()) 
  })
  
  year_5_total_c5 <- reactive({
    get_total_salary(scales_c_year5_5())
  })
  
  

  plot_data5 <- reactive({
    tibble::tibble(
      op_a = c(
        year_1_total_a5(),
        year_2_total_a5(),
        year_3_total_a5(),
        year_4_total_a5(),
        year_5_total_a5()
      ),
      op_b = c(
        year_1_total_b5(),
        year_2_total_b5(),
        year_3_total_b5(),
        year_4_total_b5(),
        year_5_total_b5()
      ),
      op_c = c(
        year_1_total_c5(),
        year_2_total_c5(),
        year_3_total_c5(),
        year_4_total_c5(),
        year_5_total_c5()
      )
    )
  })



  #Render plot with plotly

  output$budget_plot5 <- plotly::renderPlotly({
    plotly::plot_ly(
      plot_data5(),
      x = ~ c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5'),
      y = ~ op_a,
      type = 'bar',
      name = 'Option A',
      marker = list(color = '#DB9743'),
      hovertemplate = 'Salaries: %{y:$,.0f}<extra></extra>',
      texttemplate = '$%{y:.3s}',
      textposition = 'inside',
      height = 500
    ) %>%
      plotly::add_trace(y = ~ op_b,
                        name = 'Option B',
                        marker = list(color = '#2a3a6e')) %>%
      plotly::add_trace(y = ~ op_c,
                        name = 'Option C',
                        marker = list(color = 'grey')) %>%
      plotly::layout(
        xaxis = list(title = "", tickangle = -45, fixedrange = T),
        yaxis = list(title = "", range = c(max(plot_data5())*0.66, max(plot_data5()*1.05)), fixedrange = T),
        margin = list(b = 100),
        barmode = 'group',
        showlegend =F,
        font = list(size = 18),
        images = list(
          source = "https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png",
          x = -0.03, y = -0.1,
          sizex = 0.20, sizey = 0.10
        )
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      ) %>%
      plotly::add_annotations(
        text= glue::glue('Current {language()} Salaries: {payroll()}'),
        align='left',
        showarrow= F,
        xref='paper',
        yref='paper',
        x=0.02,
        y=0.99,
        bordercolor='black',
        borderwidth=1
      )
  })
  detail_table5 <- reactive({
    tibble::tibble(
      option= c('Option A', 'Option B', 'Option C'),
      cola_year_1 = c(
        input$cola1_a5,
        input$cola1_b5,
        input$cola1_c5
      ),
      salaries_year_1 = c(
        round(year_1_total_a5()),
        round(year_1_total_b5()),
        round(year_1_total_c5())
      ),
      perc_increase_1  =c(
        round((((year_1_total_a5())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_1_total_b5())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_1_total_c5())-payroll_raw())*100)/payroll_raw(),2 )
      ),
      cola_year_2 = c(
        input$cola2_a5,
        input$cola2_b5,
        input$cola2_c5
      ),
      salaries_year_2 = c(
        round(year_2_total_a5()),
        round(year_2_total_b5()),
        round(year_2_total_c5())
      ),
      perc_increase_2  =c(
        round((((year_2_total_a5())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_2_total_b5())-payroll_raw())*100)/payroll_raw(),2 ),
        round((((year_2_total_c5())-payroll_raw())*100)/payroll_raw(),2 )
      ),
      cola_year_3 = c(
        input$cola3_a5,
        input$cola3_b5,
        input$cola3_c5
      ),
      salaries_year_3 = c(
        round(year_3_total_a5()),
        round(year_3_total_b5()),
        round(year_3_total_c5())
      ),
      perc_increase_3  =c(
        round(((year_3_total_a5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_3_total_b5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_3_total_c5()-payroll_raw())*100)/payroll_raw(),2 )
      ),
      cola_year_4 = c(
        input$cola4_a5,
        input$cola4_b5,
        input$cola4_c5
      ),
      salaries_year_4 = c(
        round(year_4_total_a5()),
        round(year_4_total_b5()),
        round(year_4_total_c5())
      ),
      perc_increase_4  =c(
        round(((year_4_total_a5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_4_total_b5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_4_total_c5()-payroll_raw())*100)/payroll_raw(),2 )
      ),
      cola_year_5 = c(
        input$cola5_a5,
        input$cola5_b5,
        input$cola5_c5
      ),
      salaries_year_5 = c(
        round(year_5_total_a5()),
        round(year_5_total_b5()),
        round(year_5_total_c5())
      ),
      perc_increase_5  =c(
        round(((year_5_total_a5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_5_total_b5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_5_total_c5()-payroll_raw())*100)/payroll_raw(),2 )
      )
    )
  })
  
  table_data5 <- reactive({
    tibble::tibble(
      
      option = c('Option A', 'Option B', 'Option C'),
      
      total_salaries = c(
        round(year_5_total_a5()),
        round(year_5_total_b5()),
        round(year_5_total_c5())
      ),
      total_increase = c(
        round(year_5_total_a5() - payroll_raw()),
        round(year_5_total_b5() - payroll_raw()),
        round(year_5_total_c5() - payroll_raw())
      ),
      perc_increase = c(
        round(((year_5_total_a5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_5_total_b5()-payroll_raw())*100)/payroll_raw(),2 ),
        round(((year_5_total_c5()-payroll_raw())*100)/payroll_raw(),2 )
      )
    )
  })
  
  
  cola_increase_a5 <- reactive({
    year_5_total_a5() - # Final Total Amount
      round(sum(scales_a_year5_5()$fte*active_scales()$salary, na.rm = T) + # Effect from FTE
              # Effect Stipends on 1
              get_total_salary(scales_a_year1_5()) - sum(scales_a_year1_5()$fte*active_scales()$salary*cola_a1_5(), na.rm = T) +  
              # Effect Stipends on 2
              get_total_salary(scales_a_year2_5()) - sum(scales_a_year2_5()$fte*scales_a_year1_5()$salary*cola_a2_5(), na.rm = T) + 
              # Effect Stipends on 3
              get_total_salary(scales_a_year3_5()) - sum(scales_a_year3_5()$fte*scales_a_year2_5()$salary*cola_a3_5(), na.rm = T) + 
              # Effect Stipends on 4
              get_total_salary(scales_a_year4_5()) - sum(scales_a_year4_5()$fte*scales_a_year3_5()$salary*cola_a4_5(), na.rm = T) + 
              # Effect Stipends on 5
              get_total_salary(scales_a_year5_5()) - sum(scales_a_year5_5()$fte*scales_a_year4_5()$salary*cola_a5_5(), na.rm = T)
            )  
  })
  
  cola_increase_b5 <- reactive({
    year_5_total_b5() -
      round(sum(scales_b_year5_5()$fte*active_scales()$salary, na.rm = T) + # Effect from FTE
              # Effect Stipends on 1
              get_total_salary(scales_b_year1_5()) - sum(scales_b_year1_5()$fte*active_scales()$salary*cola_b1_5(), na.rm = T) +  
              # Effect Stipends on 2
              get_total_salary(scales_b_year2_5()) - sum(scales_b_year2_5()$fte*scales_b_year1_5()$salary*cola_b2_5(), na.rm = T) + 
              # Effect Stipends on 3
              get_total_salary(scales_b_year3_5()) - sum(scales_b_year3_5()$fte*scales_b_year2_5()$salary*cola_b3_5(), na.rm = T) + 
              # Effect Stipends on 4
              get_total_salary(scales_b_year4_5()) - sum(scales_b_year4_5()$fte*scales_b_year3_5()$salary*cola_b4_5(), na.rm = T) + 
              # Effect Stipends on 5
              get_total_salary(scales_b_year5_5()) - sum(scales_b_year5_5()$fte*scales_b_year4_5()$salary*cola_b5_5(), na.rm = T)
      )
  })
  
  cola_increase_c5 <- reactive({
    year_5_total_c5() -
      round(sum(scales_c_year5_5()$fte*active_scales()$salary, na.rm = T) + # Effect from FTE
              # Effect Stipends on 1
              get_total_salary(scales_c_year1_5()) - sum(scales_c_year1_5()$fte*active_scales()$salary*cola_c1_5(), na.rm = T) +  
              # Effect Stipends on 2
              get_total_salary(scales_c_year2_5()) - sum(scales_c_year2_5()$fte*scales_c_year1_5()$salary*cola_c2_5(), na.rm = T) + 
              # Effect Stipends on 3
              get_total_salary(scales_c_year3_5()) - sum(scales_c_year3_5()$fte*scales_c_year2_5()$salary*cola_c3_5(), na.rm = T) + 
              # Effect Stipends on 4
              get_total_salary(scales_c_year4_5()) - sum(scales_c_year4_5()$fte*scales_c_year3_5()$salary*cola_c4_5(), na.rm = T) + 
              # Effect Stipends on 5
              get_total_salary(scales_c_year5_5()) - sum(scales_c_year5_5()$fte*scales_c_year4_5()$salary*cola_c5_5(), na.rm = T)
      )
  })
  
  breakdown_table5 <- reactive({
    tibble::tibble(
      option = c('Option A', 'Option B', 'Option C'),
      
      total_increase = c(
        round(year_5_total_a5() - payroll_raw()),
        round(year_5_total_b5() - payroll_raw()),
        round(year_5_total_c5() - payroll_raw())
      ),
      cola_increase_total = c(
        round(cola_increase_a5()),
        round(cola_increase_b5()),
        round(cola_increase_c5())
        
      ),
      increase_from_cola = c(
        round(100*cola_increase_a5()/(year_5_total_a5() - payroll_raw()),2),
        round(100*cola_increase_b5()/(year_5_total_b5() - payroll_raw()),2),
        round(100*cola_increase_c5()/(year_5_total_c5() - payroll_raw()),2)
      ),
      step_increase_total = c(
        round(sum(scales_a_year5_5()$fte*active_scales()$salary, na.rm = T) - payroll_raw()),
        round(sum(scales_b_year5_5()$fte*active_scales()$salary, na.rm = T) - payroll_raw()),
        round(sum(scales_c_year5_5()$fte*active_scales()$salary, na.rm = T) - payroll_raw())
        
      ),
      increase_from_steps = c(
        round(100*(sum(scales_a_year5_5()$fte*active_scales()$salary, na.rm = T) - payroll_raw())/
                (year_5_total_a5() - payroll_raw()),2),
        round(100*(sum(scales_b_year5_5()$fte*active_scales()$salary, na.rm = T) - payroll_raw())/
                (year_5_total_b5() - payroll_raw()),2),
        round(100*(sum(scales_c_year5_5()$fte*active_scales()$salary, na.rm = T) - payroll_raw())/
                (year_5_total_c5() - payroll_raw()),2)
        
      ),
      other_total = c(
        (year_1_total_a5()- 
           sum(scales_a_year1_5()$fte*active_scales()$salary*cola_a1_5(), na.rm = T) + 
           year_2_total_a5()- sum(scales_a_year2_5()$fte*scales_a_year1_5()$salary*cola_a2_5(), na.rm = T) + 
           year_3_total_a5()- sum(scales_a_year3_5()$fte*scales_a_year2_5()$salary*cola_a3_5(), na.rm = T) +
           year_4_total_a5()- sum(scales_a_year4_5()$fte*scales_a_year3_5()$salary*cola_a4_5(), na.rm = T)+
           year_5_total_a5()- sum(scales_a_year5_5()$fte*scales_a_year4_5()$salary*cola_a5_5(), na.rm = T))%>% 
          round(),
        (year_1_total_b5()- 
           sum(scales_b_year1_5()$fte*active_scales()$salary*cola_b1_5(), na.rm = T) + 
           year_2_total_b5()- sum(scales_b_year2_5()$fte*scales_b_year1_5()$salary*cola_b2_5(), na.rm = T) + 
           year_3_total_b5()- sum(scales_b_year3_5()$fte*scales_b_year2_5()$salary*cola_b3_5(), na.rm = T) +
           year_4_total_b5()- sum(scales_b_year4_5()$fte*scales_b_year3_5()$salary*cola_b4_5(), na.rm = T)+
           year_5_total_b5()- sum(scales_b_year5_5()$fte*scales_b_year4_5()$salary*cola_b5_5(), na.rm = T))%>% 
          round(),
        (year_1_total_c5()- 
           sum(scales_c_year1_5()$fte*active_scales()$salary*cola_c1_5(), na.rm = T) + 
           year_2_total_c5()- sum(scales_c_year2_5()$fte*scales_c_year1_5()$salary*cola_c2_5(), na.rm = T) + 
           year_3_total_c5()- sum(scales_c_year3_5()$fte*scales_c_year2_5()$salary*cola_c3_5(), na.rm = T) +
           year_4_total_c5()- sum(scales_c_year4_5()$fte*scales_c_year3_5()$salary*cola_c4_5(), na.rm = T)+
           year_5_total_c5()- sum(scales_c_year5_5()$fte*scales_c_year4_5()$salary*cola_c5_5(), na.rm = T))%>% 
          round()
        
      ),
      others = c(
        (100*(year_1_total_a5()- 
                sum(scales_a_year1_5()$fte*active_scales()$salary*cola_a1_5(), na.rm = T) + 
                year_2_total_a5()- sum(scales_a_year2_5()$fte*scales_a_year1_5()$salary*cola_a2_5(), na.rm = T) + 
                year_3_total_a5()- sum(scales_a_year3_5()$fte*scales_a_year2_5()$salary*cola_a3_5(), na.rm = T) + 
                year_4_total_a5()- sum(scales_a_year4_5()$fte*scales_a_year3_5()$salary*cola_a4_5(), na.rm = T) + 
                year_5_total_a5()- sum(scales_a_year5_5()$fte*scales_a_year4_5()$salary*cola_a5_5(), na.rm = T))/
           (year_3_total_a() - payroll_raw()))%>% 
          round(2),
        (100*(year_1_total_b5()- 
                sum(scales_b_year1_5()$fte*active_scales()$salary*cola_b1_5(), na.rm = T) + 
                year_2_total_b5()- sum(scales_b_year2_5()$fte*scales_b_year1_5()$salary*cola_b2_5(), na.rm = T) + 
                year_3_total_b5()- sum(scales_b_year3_5()$fte*scales_b_year2_5()$salary*cola_b3_5(), na.rm = T) + 
                year_4_total_b5()- sum(scales_b_year4_5()$fte*scales_b_year3_5()$salary*cola_b4_5(), na.rm = T) + 
                year_5_total_b5()- sum(scales_b_year5_5()$fte*scales_b_year4_5()$salary*cola_b5_5(), na.rm = T))/
           (year_3_total_b() - payroll_raw()))%>% 
          round(2),
        (100*(year_1_total_c5()- 
                sum(scales_c_year1_5()$fte*active_scales()$salary*cola_c1_5(), na.rm = T) + 
                year_2_total_c5()- sum(scales_c_year2_5()$fte*scales_c_year1_5()$salary*cola_c2_5(), na.rm = T) + 
                year_3_total_c5()- sum(scales_c_year3_5()$fte*scales_c_year2_5()$salary*cola_c3_5(), na.rm = T) + 
                year_4_total_c5()- sum(scales_c_year4_5()$fte*scales_c_year3_5()$salary*cola_c4_5(), na.rm = T) + 
                year_5_total_c5()- sum(scales_c_year5_5()$fte*scales_c_year4_5()$salary*cola_c5_5(), na.rm = T))/
           (year_3_total_c() - payroll_raw()))%>% 
          round(2)
      )
    )
  })

  years_table_a5 <- reactive({
    dplyr::bind_cols('Cola 1' = input$cola1_a5,
                     reshape2::acast(scales_a_year1_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 1 - Lane', x)),
                     'Cola 2' = input$cola2_a5,
                     reshape2::acast(scales_a_year2_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 2 - Lane', x)),
                     'Cola 3' = input$cola3_a5,
                     reshape2::acast(scales_a_year3_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x)),
                     'Cola 4' = input$cola4_a5,
                     reshape2::acast(scales_a_year4_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x)),
                     'Cola 5' = input$cola5_a5,
                     reshape2::acast(scales_a_year5_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x))
    ) %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with('Year'), function(x){round(as.numeric(x))}))
    
  })
  
  years_table_b5 <- reactive({
    dplyr::bind_cols('Cola 1' = input$cola1_b5,
                     reshape2::acast(scales_b_year1_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 1 - Lane', x)),
                     'Cola 2' = input$cola2_b5,
                     reshape2::acast(scales_b_year2_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 2 - Lane', x)),
                     'Cola 3' = input$cola3_b5,
                     reshape2::acast(scales_b_year3_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x)),
                     'Cola 4' = input$cola4_b5,
                     reshape2::acast(scales_b_year4_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x)),
                     'Cola 5' = input$cola5_b5,
                     reshape2::acast(scales_b_year5_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x))
    ) %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with('Year'), function(x){round(as.numeric(x))}))
    
  })
  
  years_table_c5 <- reactive({
    dplyr::bind_cols('Cola 1' = input$cola1_c5,
                     reshape2::acast(scales_c_year1_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 1 - Lane', x)),
                     'Cola 2' = input$cola2_c5,
                     reshape2::acast(scales_c_year2_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 2 - Lane', x)),
                     'Cola 3' = input$cola3_c5,
                     reshape2::acast(scales_c_year3_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x)),
                     'Cola 4' = input$cola4_c5,
                     reshape2::acast(scales_c_year4_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x)),
                     'Cola 5' = input$cola5_c5,
                     reshape2::acast(scales_c_year5_5(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                                     value.var = 'salary', fill = '0') %>% 
                       tibble::as_tibble() %>% 
                       dplyr::rename_with(function(x)paste('Year 3 - Lane', x))
    ) %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with('Year'), function(x){round(as.numeric(x))}))
    
  })
  

  
  #download handlers for all csv and png downloads

  output$down_summ5 = downloadHandler(
    filename = function() {
      paste("Salary Summary (5-year)", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_data5(), file, row.names = F)
    }
  )

  output$down_det5 = downloadHandler(
    filename = function() {
      paste("Salary Details (5-Year)", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(detail_table5(), file, row.names = F)
    }
  )

  output$down_break5 = downloadHandler(
    filename = function() {
      paste("Salary Breakdown (5-Year) ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(breakdown_table(), file, row.names = F)
    }
  )

  output$down_years_a5 = downloadHandler(
    filename = function() {
      paste("Salary Scales (A-5) ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(years_table_a5(), file, row.names = F)
    }
  )

  output$down_years_b5 = downloadHandler(
    filename = function() {
      paste("Salary Scales (B-5) ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(years_table_b5(), file, row.names = F)
    }
  )

  output$down_years_c5 = downloadHandler(
    filename = function() {
      paste("Salary Scales (C-5) ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(years_table_c5(), file, row.names = F)
    }
  )


  #displayed gt tables


  summary_tab5 <- reactive({
    table_data5() %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::md("**Five-Year Projections**"),
        subtitle = gt::html(paste0(tags$span(style = "font-size:15px;text-align:left;" , 'Current', language(), 'Salaries: ', payroll())))
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#DB9743')
        ),
        locations = gt::cells_body(rows = 1)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#2a3a6e')
        ),
        locations = gt::cells_body(rows = 2)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#9399a3')
        ),
        locations = gt::cells_body(rows = 3)
      ) %>%
      gt::fmt_number(columns = c(total_salaries, total_increase),
                     use_seps  = T,
                     sep_mark = ',',
                     pattern = '${x}',
                     decimals = 0
      ) %>%
      gt::fmt_number(columns = c(perc_increase),
                     decimals = 1,
                     patter = '{x}%') %>%
      gt::tab_source_note(
        source_note = gt::html(paste0(tags$span(style = "font-size:17px;" , tags$span(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png',width='50px', style = 'margin-right:3px;margin-bottom:-3px;')), "LITIX", 'Budget Forecasting')))
      ) %>%
      # gt::tab_source_note(
      #     source_note = gt::html(paste0(shinyMobile::f7Link(href = 'https://www.arxed.com/', label = 'Visit Our Website')))
      #     ) %>%
      gt::cols_width(
        dplyr::everything() ~ gt::pct(20)
      ) %>%
      gt::cols_label(
        total_salaries = "Total Salaries",
        total_increase = "5-yr Salary Increase",
        perc_increase = "5-yr % Salary Increase",
        option = 'Proj. Option'
      ) %>%
      # gt::opt_table_font(
      # font = list(
      # gt::google_font("Roboto Mono"),
      #          gt::default_fonts()),
      # weight = 400) %>%
      gt::tab_options(column_labels.font.size = '100%',
                      column_labels.font.weight = '600') %>%
      gt::cols_align(align = c("center"),
                     columns = c(option:perc_increase)) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_title("subtitle")
      )
  })

  details_tab5 <- reactive({
    detail_table5() %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::md("**Five-Year Projections (Detailed)**"),
        subtitle = gt::html(paste0(tags$span(style = "font-size:15px;text-align:left;" , 'Current',language(), 'Salaries: ', payroll())))
      ) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_title("subtitle")
      ) %>%
      gt::tab_spanner(
        label = "Year 1",
        columns = c(cola_year_1, salaries_year_1, perc_increase_1)
      ) %>%
      gt::tab_spanner(
        label = "Year 2",
        columns = c(cola_year_2, salaries_year_2, perc_increase_2)
      ) %>%
      gt::tab_spanner(
        label = "Year 3",
        columns = c(cola_year_3, salaries_year_3, perc_increase_3)
      ) %>%
      gt::tab_spanner(
        label = "Year 4",
        columns = c(cola_year_4, salaries_year_4, perc_increase_4)
      ) %>%
      gt::tab_spanner(
        label = "Year 5",
        columns = c(cola_year_5, salaries_year_5, perc_increase_5)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#DB9743')
        ),
        locations = gt::cells_body(rows = 1)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#2a3a6e')
        ),
        locations = gt::cells_body(rows = 2)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#9399a3')
        ),
        locations = gt::cells_body(rows = 3)
      ) %>%
      gt::fmt_number(
        columns = c(
          salaries_year_1,
          salaries_year_2,
          salaries_year_3,
          salaries_year_4,
          salaries_year_5
        ),
        use_seps  = T,
        sep_mark = ',',
        pattern = '${x}',
        decimals = 0
      ) %>%
      gt::fmt_number(
        columns = c(
          perc_increase_1,
          perc_increase_2,
          perc_increase_3,
          perc_increase_4,
          perc_increase_5
        ),
        decimals = 1,
        patter = '{x}%') %>%
      gt::fmt_number(
        columns = c(
          cola_year_1,
          cola_year_2,
          cola_year_3,
          cola_year_4,
          cola_year_5
        ),
        decimals = 2,
        patter = '{x}%') %>% 
      gt::tab_source_note(
        source_note = gt::html(paste0(tags$span(style = "font-size:17px;" , tags$span(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png',width='50px', style = 'margin-right:3px;margin-bottom:-3px;')), "LITIX", 'Budget Forecasting')))
      ) %>%
      gt::cols_label(
        cola_year_1 = "COLA",
        salaries_year_1 = "Salary",
        perc_increase_1 = "% Increase",
        cola_year_2 = "COLA",
        salaries_year_2 = "Salary",
        perc_increase_2 = "% Increase",
        cola_year_3 = "COLA",
        salaries_year_3 = "Salary",
        perc_increase_3 = "% Increase",
        cola_year_4 = "COLA",
        salaries_year_4 = "Salary",
        perc_increase_4 = "% Increase",
        cola_year_5 = "COLA",
        salaries_year_5 = "Salary",
        perc_increase_5 = "% Increase",
        # total_salary = 'Total 3-Yr Salary',
        option = 'Proj. Option'
      ) %>%
      gt::cols_width(
        dplyr::everything() ~ gt::pct(10)
      ) %>%
      # gt::opt_table_font(
      #    font = list(gt::google_font("Roboto Mono"),
      #                gt::default_fonts()),
      #                weight = 400) %>%
      gt::tab_options(column_labels.font.size = '100%',
                      column_labels.font.weight = '600') %>%
      gt::cols_align(align = c("center"),
                     columns = c(option:perc_increase_3)) %>%
      gtExtras::gt_add_divider(
        sides = 'left',
        columns = c(cola_year_2, cola_year_3, cola_year_4,cola_year_5 ),
        color = '#d8d4d4'
      )
  })


  breakdown_tab5 <- reactive({
    breakdown_table5() %>%
      gt::gt() %>%
      gt::tab_header(
        title = gt::md("**Five-Year Increase Breakdown**"),
        subtitle = gt::html(paste0(tags$span(style = "font-size:15px;text-align:left;" ,
                                             'Current', language(), 'Salaries: ', payroll())))
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#DB9743')
        ),
        locations = gt::cells_body(rows = 1)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#2a3a6e')
        ),
        locations = gt::cells_body(rows = 2)
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "600", color = '#9399a3')
        ),
        locations = gt::cells_body(rows = 3)
      ) %>%
      gt::fmt_number(columns = c(total_increase, cola_increase_total, step_increase_total, other_total),
                     use_seps  = T,
                     sep_mark = ',',
                     pattern = '${x}',
                     decimals = 0
      ) %>%
      gt::fmt_number(columns = c(increase_from_cola, increase_from_steps, others),
                     decimals = 1,
                     patter = '{x}%') %>%
      gt::tab_source_note(
        source_note = gt::html(paste0(tags$span(style = "font-size:17px;" , tags$span(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png',width='50px', style = 'margin-right:3px;margin-bottom:-3px;')), "LITIX", 'Budget Forecasting')))
      ) %>%
      gt::cols_width(
        dplyr::everything() ~ gt::pct(20)
      ) %>%
      gt::cols_label(
        total_increase = "Total 5-Year Increase",
        cola_increase_total = "Total Increase from COLA",
        increase_from_cola = "% of Increase from COLA",
        step_increase_total = "Total Increase from Step Progression",
        increase_from_steps = "% of Increase from Steps",
        other_total = 'Others (Total)',
        others = 'Others',
        option = 'Proj. Option'
      ) %>%
      gt::tab_options(column_labels.font.size = '100%',
                      column_labels.font.weight = '600') %>%
      gt::cols_align(align = c("center"),
                     columns = c(option:others)) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_title("subtitle")
      )

  })

  output$sum_table5 <- gt::render_gt({
    summary_tab5()
  })

  output$det_table5 <- gt::render_gt({
    details_tab5()
  })

  output$break_table5 <- gt::render_gt({
    breakdown_tab5()
  })

  #Conditional UI that switches between plot and tables

  output$table_plot5 <- renderUI({
    if(input$toggle5 %% 2 == 0){
      tags$div(
        plotly::plotlyOutput('budget_plot5', height = 'auto'),
        tags$div(
          shinyMobile::f7Button(
            inputId = 'down_chart5',
            label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Bar Chart')) #fas fa classes corresponf to fontawesome
          ),
          class = "other-button card-button2"))
    } else if(input$toggle5 %% 2 == 1){
      tags$div(tags$div(
        tags$div(shinyMobile::f7Button(
          inputId = 'down_summ_img5',
          label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Summary (PNG)'))
        ),
        class = "other-button card-button"),
        gt::gt_output('sum_table5')),
        tags$div(
          tags$div(shinyMobile::f7Button(
            inputId = 'down_det_img5',
            label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Details (PNG)'))
          ),
          class = "other-button card-button"),
          gt::gt_output('det_table5')),
        tags$div(
          tags$div(shinyMobile::f7Button(
            inputId = 'down_break_img5',
            label = tags$span(tags$i(class = 'fas fa-download'), tags$span('Breakdown (PNG)'))
          ),
          class = "other-button card-button"),
          gt::gt_output('break_table5')))
    } else{
    }
  })


  observeEvent(input$down_chart5, { #take "screenshot" of plot selector
    shinyscreenshot::screenshot(selector = '#budget_plot5' , filename = 'Salary Chart (5 Years) ', scale = 4)
  })

  observeEvent(input$down_summ_img5, { #take "screenshot" of table selector
    shinyscreenshot::screenshot(selector = '#sum_table5' , filename = 'Summary Table', scale = 4)
  })

  observeEvent(input$down_det_img5, {
    shinyscreenshot::screenshot(selector = '#det_table5' , filename = 'Details Table', scale = 4)
  })

  observeEvent(input$down_break_img5, {
    shinyscreenshot::screenshot(selector = '#break_table5' , filename = 'Breakdown Table', scale = 4)
  })

  
  # Heatmap Tab --------------------------------------------------------------

  heatmap_data1 <- reactive({ 
    temp <- reshape2::acast(active_scales() %>% dplyr::filter(salary != 0), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                    value.var = 'fte', fill = 'NA') %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::everything(), function(x)as.numeric(x)))
    temp/total_fte()*100
  })


  heatmap_data2 <- reactive({
    temp <- reshape2::acast(scales_a_year1(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                           value.var = 'fte', fill = '0') %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::everything(), function(x)as.numeric(x)))
    temp/total_fte()*100
  })

  heatmap_data3 <- reactive({
    temp <- reshape2::acast(scales_a_year2(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                           value.var = 'fte', fill = '0') %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::everything(), function(x)as.numeric(x)))
    temp/total_fte()*100
  })

  heatmap_data4 <- reactive({
    temp <- reshape2::acast(scales_a_year3(), row_num ~ col_num, function(x) {sort(as.character(x))[1]},
                           value.var = 'fte', fill = '0') %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::everything(), function(x)as.numeric(x)))
    temp/total_fte()*100
  })
  
  bottom_step_y1 <- reactive({ #calculate total fte
    temp <- scales_a_year1() %>%
      dplyr::filter(salary != 0 & !is.na(salary)) %>%
      dplyr::group_by(col_num) %>%
      dplyr::filter(row_num == max(row_num)) %>%
      dplyr::ungroup()
    
    paste0(round((sum(temp$fte)/total_fte())*100, 1),'%')
  })
  
  bottom_step_y2 <- reactive({ #calculate total fte
    temp <- scales_a_year2() %>%
      dplyr::filter(salary != 0 & !is.na(salary)) %>%
      dplyr::group_by(col_num) %>%
      dplyr::filter(row_num == max(row_num)) %>%
      dplyr::ungroup()
    
    paste0(round((sum(temp$fte)/total_fte())*100, 1),'%')
  })
  
  bottom_step_y3 <- reactive({ #calculate total fte
    temp <- scales_a_year3() %>%
      dplyr::filter(salary != 0 & !is.na(salary)) %>%
      dplyr::group_by(col_num) %>%
      dplyr::filter(row_num == max(row_num)) %>%
      dplyr::ungroup()
    
    paste0(round((sum(temp$fte)/total_fte())*100, 1),'%')
  })
  

  #Standard plotly heatmaps with percentage data

  output$heatmap1 <- plotly::renderPlotly(
    plotly::plot_ly(
      x = purrr::map_chr(seq(ncol(heatmap_data1())), function(x){paste('Lane', x)}), #cleaner name
      y = seq_len(nrow(heatmap_data1())),
      z = heatmap_data1() %>% as.matrix(),
      type = 'heatmap',
      colors = colorRamp(c('white', '#2545cc', '#142c8f', '#101f5c'), bias = 0.2), #The bias allows more distinction for the higher values
      hovertemplate = 'Step: %{y:,.0f} \nFTE: %{z:,.1f}%<extra></extra>',
    ) %>%
      plotly::colorbar(limits = c(0,max(heatmap_data4()))) %>% #Important so that al heatmaps follow the same color scale (same maximum)
      plotly::layout(
        title = 'Current Year FTE %',
        yaxis = list(fixedrange = T, autorange = 'reversed'), #reversed so lanes go from 1-16 descending
        xaxis = list(fixedrange = T),
        margin = list(l= 50, r = 0, t = 50, b = 0, pad = 4),
        images = list(
          source = "https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png",
          x = 0, y = 1.15,
          sizex = 0.25, sizey = 0.15
        )
      ) %>% 
      plotly::add_annotations(
        text = paste('% Highest Step:',  paste0(round((bottom_step()/total_fte())*100, 1),'%')),
        align = 'left',
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        x = 1,
        y = 1.1,
        bordercolor = 'black',
        borderwidth = 1
      )
  ) 


  output$heatmap2 <- plotly::renderPlotly(
    plotly::plot_ly(
      x = purrr::map_chr(seq(ncol(heatmap_data2())), function(x){paste('Lane', x)}),
      y = seq(nrow(heatmap_data2())),
      z = heatmap_data2() %>% as.matrix(),
      type = 'heatmap',
      colors = colorRamp(c('white', '#2545cc', '#142c8f', '#101f5c'), bias = 0.2),
      hovertemplate = 'Step: %{y:,.0f} \nFTE: %{z:,.1f}%<extra></extra>',
    ) %>%
      plotly::colorbar(limits = c(0,max(heatmap_data4()))) %>%
      plotly::layout(
        title = 'Projected Year 1 FTE',
        yaxis = list(fixedrange = T, autorange = 'reversed'),
        xaxis = list(fixedrange = T),
        margin = list(l= 40, r = 0, t = 50, b = 0, pad = 4),
        images = list(
          source = "https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png",
          x = 0, y = 1.15,
          sizex = 0.25, sizey = 0.15
        )
      ) %>% 
      plotly::add_annotations(
        text = paste('% Highest Step:',  bottom_step_y1()),
        align = 'left',
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        x = 1,
        y = 1.1,
        bordercolor = 'black',
        borderwidth = 1
      )
  )

  output$heatmap3 <- plotly::renderPlotly(
    plotly::plot_ly(
      x = purrr::map_chr(seq(ncol(heatmap_data2())), function(x){paste('Lane', x)}),
      y = seq(nrow(heatmap_data3())),
      z = heatmap_data3() %>% as.matrix(),
      type = 'heatmap',
      colors = colorRamp(c('white', '#2545cc', '#142c8f', '#101f5c'), bias = 0.2),
      hovertemplate = 'Step: %{y:,.0f} \nFTE: %{z:,.1f}%<extra></extra>',
    ) %>%
      plotly::colorbar(limits = c(0,max(heatmap_data4()))) %>%
      plotly::layout(
        title = 'Projected Year 2 FTE',
        yaxis = list(fixedrange = T, autorange = 'reversed'),
        xaxis = list(fixedrange = T),
        margin = list(l= 40, r = 0, t = 50, b = 0, pad = 4),
        images = list(
          source = "https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png",
          x = 0, y = 1.15,
          sizex = 0.25, sizey = 0.15
        )
      ) %>% 
      plotly::add_annotations(
        text = paste('% Highest Step:',  bottom_step_y2()),
        align = 'left',
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        x = 1,
        y = 1.1,
        bordercolor = 'black',
        borderwidth = 1
      )
  )

  output$heatmap4 <- plotly::renderPlotly(
    plotly::plot_ly(
      x = purrr::map_chr(seq(ncol(heatmap_data2())), function(x){paste('Lane', x)}),
      y = seq(nrow(heatmap_data4())),
      z = heatmap_data4() %>% as.matrix(),
      type = 'heatmap',
      colors = colorRamp(c('white', '#2545cc', '#142c8f', '#101f5c'), bias = 0.2),
      hovertemplate = 'Step: %{y:,.0f} \nFTE: %{z:,.1f}%<extra></extra>',
    ) %>%
      plotly::colorbar(limits = c(0,max(heatmap_data4()))) %>%
      plotly::layout(
        title = 'Projected Year 3 FTE',
        yaxis = list(fixedrange = T, autorange = 'reversed'),
        xaxis = list(fixedrange = T),
        margin = list(l= 40, r = 0, t = 50, b = 0, pad = 4),
        images = list(
          source = "https://raw.githubusercontent.com/SCasanova/arxed/main/arxed%20logos/LITIX%20PNG/LITIX%20TM%20Main%20Logo%20-%20Transparent%20BG.png",
          x = 0, y = 1.15,
          sizex = 0.25, sizey = 0.15
        )
      ) %>% 
      plotly::add_annotations(
        text = paste('% Highest Step:',  bottom_step_y3()),
        align = 'left',
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        x = 1,
        y = 1.1,
        bordercolor = 'black',
        borderwidth = 1
      )
  )

  observeEvent(input$down_heatmap, { #Screenshot the all_heatmaps selector to get a PNG
    shinyscreenshot::screenshot(selector = '#all_heatmaps' , filename = 'Heatmaps', scale = 4)
  })


  
}



shinyApp(ui = ui, server = server)
