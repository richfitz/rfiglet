cfiglet <- function(str, font="standard", capture=TRUE) {
  filename <- figlet_font_filename(figlet_font_name(font))
  filename <- sprintf('"%s"', filename)
  args <- c("-f", filename, "-w", 222, str)
  system2("figlet/figlet", args, stdout=capture)
}

cfiglet_available <- function() {
  file.exists("figlet/figlet")
}

skip_if_no_cfiglet <- function() {
  if (cfiglet_available()) {
    return()
  }
  skip("cfiglet not installed")
}

trim_left <- function(x) {
  m <- strsplit(x, NULL)
  m <- do.call("rbind", m, quote=TRUE)
  i <- min(which(colSums(m != " ") > 0))
  m <- m[, i:ncol(m), drop=FALSE]
  apply(m, 1, paste0, collapse="")
}
