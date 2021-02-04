#!/usr/bin/env Rscript
pkgload::load_all()
registry_clear()
registry_load_dir(rfiglet_file("fonts"))
registry_builtin <- registry
save(registry_builtin, file = file.path(here::here(), "R/sysdata.rda"),
     version = 2L)
