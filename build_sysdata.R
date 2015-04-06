#!/usr/bin/env Rscript
devtools::load_all()
figlet_base_data <- build_font_info("inst/fonts")
figlet_base_data$collection <- "ours"
save(figlet_base_data, file="R/sysdata.rda")
