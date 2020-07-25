
library(ggplot2)
library(reshape2)

result_by_subject <- contestants_data %>%
            dplyr::group_by(specialized) %>%
            dplyr::summarise(TT = sum(result == 'TT'),
                             TTHB = sum(result == 'TTHB'),
                             XT = sum(result == 'XT'),
                             BYE = sum(result == ''))

result_by_subject
result_by_subject <- reshape2::melt(result_by_subject, id = 'specialized')

status <- factor(result_by_subject$variable, levels = c('BYE', 'XT', 'TT', 'TTHB'))

result_by_subject_plot <- ggplot2::ggplot(result_by_subject,
                                          ggplot2::aes(fill = status,
                                                       x = specialized,
                                                       y = value)
                                          ) +
                            ggplot2::geom_bar(position = 'stack', stat = 'identity') +
                            ggplot2::ylab('Số học sinh') + 
                            ggplot2::xlab('Môn học') +
                            ggplot2::geom_text(ggplot2::aes(label = value), size = 3, position = position_stack(vjust = 0.5)) +
                            ggplot2::ggtitle('Thống kê về kết quả chuyên ĐHSP') +
                            ggplot2::geom_text(ggplot2::aes(
                                label = stat(y),
                                group = specialized),
                                stat = 'summary',
                                fun = sum,
                                vjust = -1)

result_by_subject_plot

###############################################################################################################################

result_by_city <- contestants_data %>%
    dplyr::group_by(place) %>%
    dplyr::summarise(TT = sum(result == 'TT'),
                     TTHB = sum(result == 'TTHB'),
                     XT = sum(result == 'XT'),
                     BYE = sum(result == ''))

result_hanoi <- result_by_city[24, ]
result_by_city <- result_by_city[-24, ]
result_by_city <- result_by_city[result_by_city$TTHB > 0,]

result_by_city$place <- result_by_city$place %>%
                        sapply(function (city) {
                            sub(".*? ", "", city)
                        })
result_by_city <- reshape2::melt(result_by_city, id = 'place')

status <- factor(result_by_city$variable, levels = c('BYE', 'XT', 'TT', 'TTHB'))
result_by_city

result_by_city_plot <- ggplot2::ggplot(result_by_city,
                                          ggplot2::aes(fill = status,
                                                       x = place,
                                                       y = value)
) +
    ggplot2::geom_bar(position = 'stack', stat = 'identity') +
    ggplot2::ylab('Số học sinh') + 
    ggplot2::xlab('Tỉnh / Thành phố') +
    ggplot2::geom_text(ggplot2::aes(label = value), size = 3, position = position_stack(vjust = 0.5)) +
    ggplot2::ggtitle('Thống kê về kết quả chuyên ĐHSP theo tỉnh / thành phố' ) +
    ggplot2::geom_text(ggplot2::aes(
        label = stat(y),
        group = place),
        stat = 'summary',
        fun = sum,
        vjust = -1)

result_by_city_plot

###############################################################################################################################

result_hanoi <- reshape2::melt(result_hanoi, id = 'place')
result_hanoi <- result_hanoi[, -1]

status <- factor(result_hanoi$variable, levels = c('BYE', 'XT', 'TT', 'TTHB'))
result_hanoi

result_hanoi_plot <- ggplot2::ggplot(result_hanoi,
                                       ggplot2::aes(x = variable,
                                                    y = value,
                                                    fill = variable)
) +
    ggplot2::geom_bar(position = 'dodge', stat = 'identity') +
    ggplot2::ylab('Số học sinh') + 
    ggplot2::xlab('Kết quả') +
    ggplot2::ggtitle('Thống kê về kết quả chuyên ĐHSP  của Hà Nội ') +
    ggplot2::geom_text(ggplot2::aes(
        label = stat(y),
        group = variable),
        stat = 'summary',
        fun = sum,
        vjust = -1)

result_hanoi_plot

