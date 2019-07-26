#' Comments for R
#'
#' Gives comments with left indentation. Without capitalization when arg2 is
#' missing.
#'
#' @usage comr(arg1 = "Headline 1", arg2 = 1)
#'
#' @param arg1 String for title
#' @param arg2 Every object possible
#'
#' @examples comr("Headline 1")
#'
#' @export
comr <- function(arg1, arg2){
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



#' Centered Comments for R
#'
#' Gives centered comments. Always capitalized.
#'
#' @usage comr(arg1 = "Headline 1")
#'
#' @param arg1 String for title
#'
#' @examples comr("headline")
#'
#' @export
comr1 <- function(arg1){
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

