###########################################################################|
##########################   C O M M E N T   R   ##########################|
###########################################################################|



############################################################################
## R #######################################################################
commentr <- function(arg1,arg2){
  title <- nchar(arg1)
  filler <- 76 - title - 4
  if (!missing(arg2)){
    arg1 <- toupper(arg1)
  }
  end <- paste(c(rep("#",76), "\n", rep("#",2)," ", arg1," ",
                 rep("#",filler)),collapse = "")

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Do you want to copy into your clipboard?")

  if (choice == 1) {

    writeClipboard(end)

  } else {

    return(cat(end))

  }

} #insert someth in arg2 makes upper


commentr2 <- function(arg1){
  title <- nchar(arg1) + nchar(arg1) - 1 + 6
  filler <- 75 - title
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }
  arg1 <- toupper(arg1)
  title <- sub("\\s+$", "", gsub('(.{1})', '\\1 ', arg1))
  end <- paste(c(rep("#",75), "|", "\n", rep("#", fill1), rep(" ", 3),
                 title, rep(" ", 3), rep("#", fill2), "|", "\n",
                 rep("#", 75), "|"), collapse = "")

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Do you want to copy into your clipboard?")

  if (choice == 1) {

    writeClipboard(end)

  } else {

    return(cat(end))

  }

}



############################################################################
## LaTeX ###################################################################
commentex <- function(arg1){
  title <- nchar(arg1) + nchar(arg1) - 1 + 6
  filler <- 142 - title
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }
  arg1 <- toupper(arg1)
  title <- sub("\\s+$", "", gsub('(.{1})', '\\1 ', arg1))
  end <- paste(c(rep("%", 142), "\n", rep("%", fill1), rep(" ", 3), title,
                 rep(" ", 3), rep("%", fill2), "\n", rep("%", 142)),
               collapse = "")

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Do you want to copy into your clipboard?")

  if (choice == 1) {

    writeClipboard(end)

  } else {

    return(cat(end))

  }

}



### Other Ideas ###
# - two different latex

