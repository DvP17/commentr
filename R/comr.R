#' Centered Comments for R
#'
#' Gives centered comments. Always capitalized.
#'
#' @usage comr_h1(title = "Headline 1")
#'
#' @param title String for title
#' @param width Number of characters (integer) for width of line. Default is 80.
#'
#' @examples comr_h1("headline")
#'
#' @export
comr_h1 <- function(title, width = 80){
  # Number of title characters
  tit_n <- nchar(title) + nchar(title) - 1 + 6

  # Width is one character less
  width <- width - 1

  # Declare filler
  filler <- width - tit_n
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Capitalize title
  title <- toupper(title)
  title <- sub("\\s+$", "", gsub('(.{1})', '\\1 ', title))
  out <- paste(c(rep("#", width), "|", "\n", rep("#", fill1), rep(" ", 3),
                 title, rep(" ", 3), rep("#", fill2), "", "\n",
                 rep("#", width), "|"), collapse = "")

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Do you want to copy into your clipboard?")

  if (choice == 1) {

    writeClipboard(out)

  } else {

    return(cat(out))

  }

}


#' Comments for R
#'
#' Gives comments with left indentation. Without capitalization when arg2 is
#' missing.
#'
#' @usage comr_h2(arg1 = "Headline 1", arg2 = 1)
#'
#' @param arg1 String for title
#' @param arg2 Every object possible
#'
#' @examples comr_h2("Headline 1")
#'
#' @export
comr_h2 <- function(arg1, arg2){
  title <- nchar(arg1)
  filler <- 76 - title - 4
  if (!missing(arg2)){
    arg1 <- toupper(arg1)
  }
  end <- paste(c(rep("#", 75), "|", "\n", rep("#",2)," ", arg1," ",
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





