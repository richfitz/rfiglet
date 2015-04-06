#!/usr/bin/env Rscript
devtools::load_all()

## Load every font and dump out the options.
fonts <- figlet_fonts_in_dir()
info <- lapply(fonts, figlet_font_info)

## Then build the information that we can query on;
##   name
##   height
##   width (min, max, median)
##   smush_mode

v <- c("height", "smush_mode")
figlet_data <- t(sapply(info, function(x) unlist(x$options[v])))
rownames(figlet_data) <- fonts
save(figlet_data, file="R/sysdata.rda")
