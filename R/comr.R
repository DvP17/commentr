#' Headline 1 comment for R
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
comr_h0 <- function(title, width = 80){
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

  # Generate output
  out <- paste(c(rep("#", width), "|", "\n",
                 rep("#", fill1), rep(" ", 3),
                 title, rep(" ", 3), rep("#", fill2), "|", "\n",
                 rep("#", width), "|"), collapse = "")

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Copy output to clipboard?")

  if (choice == 1) {

    writeClipboard(out)

  } else {

    return(cat(out))

  }

}


#' Headline 2 comment for R
#'
#' Gives comments with left indentation. Without capitalization when arg2 is
#' missing.
#'
#' @usage comr_h2(arg1 = "Headline 1", capit = FALSE)
#'
#' @param title String for title.
#' @param width Number of characters (integer) for width of line. Default is 80.
#' @param align String for alignment of title. Default is "right". Other options are "left" and "center".
#' @param capit If title should be in capitals. True or false.
#' @param fill String for type of filler. Default is " " for white space. Can be any character (e.g., "#").
#'
#' @examples comr_h2("Headline 1")
#'
#' @export
comr_h1 <- function(title, width = 80, align = "left", capit = FALSE,
                    fill = " "){
  # Number of title characters
  tit_n <- nchar(title)

  # Width is one character less than margin
  width <- width - 1

  # Declare filler number
  filler <- width - tit_n - 2
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Declare filler type
  fill_type <- fill

  # Capitalize
  if (capit){
    title <- toupper(title)
  }

  # Generate output
  if (align == "left") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "#", " ", title, " ", rep(fill_type, filler - 5), "####"),
                 collapse = "")
  } else if (align == "right") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "#", rep(fill_type, filler - 5), " ", title, " ", "####"),
                 collapse = "")
  } else if (align == "center") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "#", rep(fill_type, fill1 - 1), " ", title, " ",
                   rep(fill_type, fill2 - 4), "####"), collapse = "")
  }

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Copy output to clipboard?")

  if (choice == 1) {

    writeClipboard(out)

  } else {

    return(cat(out))

  }

}


#' Headline 3 comment for R
#'
#' Gives comments with left indentation. Without capitalization when arg2 is
#' missing.
#'
#' @usage comr_h3(arg1 = "Headline 1", capit = FALSE)
#'
#' @param title String for title.
#' @param width Number of characters (integer) for width of line. Default is 80.
#' @param align String for alignment of title. Default is "right". Other options are "left" and "center".
#' @param capit If title should be in capitals. True or false.
#' @param fill String for type of filler. Default is " " for white space. Can be any character (e.g., "#").
#'
#' @examples comr_h3("Headline 1")
#'
#' @export
comr_h2 <- function(title, width = 80, align = "left", capit = FALSE,
                    fill = " "){
  # Number of title characters
  tit_n <- nchar(title)

  # Width is one character less than margin
  width <- width - 1

  # Declare filler number
  filler <- width - tit_n - 2
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Declare filler type
  fill_type <- fill

  # Capitalize
  if (capit){
    title <- toupper(title)
  }

  # Generate output
  if (align == "left") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "##", " ", title, " ", rep(fill_type, filler - 6), "####"),
                 collapse = "")
  } else if (align == "right") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "##", rep(fill_type, filler - 6), " ", title, " ", "####"),
                 collapse = "")
  } else if (align == "center") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "##", rep(fill_type, fill1 - 2), " ", title, " ",
                   rep(fill_type, fill2 - 4), "####"), collapse = "")
  }

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Copy output to clipboard?")

  if (choice == 1) {

    writeClipboard(out)

  } else {

    return(cat(out))

  }

}

#' Headline 4 comment for R
#'
#' Gives comments with left indentation. Without capitalization when arg2 is
#' missing.
#'
#' @usage comr_h4(arg1 = "Headline 1", capit = FALSE)
#'
#' @param title String for title.
#' @param width Number of characters (integer) for width of line. Default is 80.
#' @param align String for alignment of title. Default is "right". Other options are "left" and "center".
#' @param capit If title should be in capitals. True or false.
#' @param fill String for type of filler. Default is " " for white space. Can be any character (e.g., "#").
#'
#' @examples comr_h4("Headline 1")
#'
#' @export
comr_h3 <- function(title, width = 80, align = "left", capit = FALSE,
                    fill = " "){
  # Number of title characters
  tit_n <- nchar(title)

  # Width is one character less than margin
  width <- width - 1

  # Declare filler number
  filler <- width - tit_n - 2
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Declare filler type
  fill_type <- fill

  # Capitalize
  if (capit){
    title <- toupper(title)
  }

  # Generate output
  if (align == "left") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "###", " ", title, " ", rep(fill_type, filler - 7), "####"),
                 collapse = "")
  } else if (align == "right") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "###", rep(fill_type, filler - 7), " ", title, " ", "####"),
                 collapse = "")
  } else if (align == "center") {
    out <- paste(c(rep("#", width), "|", "\n",
                   "###", rep(fill_type, fill1 - 3), " ", title, " ",
                   rep(fill_type, fill2 - 4), "####"), collapse = "")
  }

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Copy output to clipboard?")

  if (choice == 1) {

    writeClipboard(out)

  } else {

    return(cat(out))

  }

}
