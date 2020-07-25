
bkup <- contestants_data
for (i in 1:ncol(contestants_data)) {
    contestants_data[[i]] <- toupper(contestants_data[[i]])
}
