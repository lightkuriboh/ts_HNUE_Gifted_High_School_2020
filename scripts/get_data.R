
install.packages('httr')
install.packages('rvest')
install.packages('dplyr')
library(httr)
library(rvest)
library(dplyr)

crawling <- function () {
    host <- 'http://ts2020.chuyensp.edu.vn/Tracuu2.asp'
    col_names <- c('name',
                   'dob',
                   'place',
                   'sbd',
                   'specialized',
                   'math',
                   'literature',
                   'special_subject',
                   'sum',
                   'result')
    maximal_sbd <- 5000
    
    result <- list()
    counter <- 1
    
    for (sbd in 1:maximal_sbd) {
        my_body <- list(txtSBD = sbd)
        res <- httr::POST(url = host, body = my_body, encode = "form")
        if (res$status_code == 200) {
            res_str <- httr::content(res, as = 'text', encoding = "UTF-8")
            my_html <- xml2::read_html(res_str)
            his_info <- my_html %>% rvest::html_nodes('td') %>% html_text()
            his_info <- his_info[-1]
            if (length(his_info) == length(col_names)) {
                names(his_info) <- col_names
                result[[counter]] <- his_info
                counter <- counter + 1
            }
        }
        if (sbd %% 20 == 0) {
            print(paste(
                as.character(
                    round(
                        100 * sbd / maximal_sbd,
                        2
                    )
                ),
                '%',
                sep ='')
            )
        }
    }
    
    dplyr::bind_rows(result)
}

contestants_data <- crawling()

nrow(contestants_data)
head(contestants_data)
