cfiglet <- function(str, font=NULL, capture=TRUE) {
  path <- system.file("fonts", package="rfiglet", mustWork=TRUE)
  args <- c("-d", path, "-w", 222)
  if (!is.null(font)) {
    args <- c(args, "-f", font)
  }
  args <- c(args, str)
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
