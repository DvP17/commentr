#' Centered Headline 0 for LaTeX
#'
#' Gives centered comments for LaTeX. Always capitalized.
#'
#' @usage comtex_h0("Headline 1")
#'
#' @param title String for title.
#' @param width Number of characters for width of caption
#'
#' @examples comtex_h0("Headline 1")
#'
#' @export
comtex_h0 <- function(title, width = 112){

  # Calculate Title Length
  tit_n <- nchar(title) + nchar(title) - 1 + 6

  # Calculate Fill Number
  filler <- width - tit_n
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2 - 1
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Capitalize
  title <- toupper(title)

  # Entire title

  # Spaces Between Letters
  title <- sub("\\s+$", "", gsub('(.{1})', '\\1 ', title))

  end <- paste(c(rep("%", width), "\n", rep("%", fill1), rep(" ", 3), title,
                 rep(" ", 3), rep("%", fill2), "\n", rep("%", width)),
               collapse = "")


  # Write to Clipboard
  choice <- menu(c("Yes", "No"), title="Copy output to clipboard?")

  if (choice == 1) {

    writeClipboard(end)

  } else {

    return(cat(end))

  }

}


#' Centered Headline 1 for LaTeX
#'
#' Gives centered comments for LaTeX. Always capitalized.
#'
#' @usage comtex_h1("Headline 1")
#'
#' @param title String for title.
#' @param width Number of characters for width of caption
#'
#' @examples comtex_h1("Headline 1")
#'
#' @export
comtex_h2 <- function(title, width = 112){

  # Calculate Title Length
  tit_n <- nchar(title) - 1 + 6

  # Calculate Fill Number
  filler <- width - tit_n
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2 - 1
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Capitalize
  title <- toupper(title)

  # Entire title
  end <- paste(c(rep("%", fill1), rep(" ", 3),
                 title,
                 rep(" ", 3), rep("%", fill2)),
               collapse = "")

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Copy output to clipboard?")

  if (choice == 1) {

    writeClipboard(end)

  } else {

    return(cat(end))

  }

}


#' Centered Headline 2 for LaTeX
#'
#' Gives centered comments for LaTeX. Always capitalized.
#'
#' @usage comtex_h2("Headline 1")
#'
#' @param title String for title.
#' @param width Number of characters for width of caption
#'
#' @examples comtex_h2("Headline 1")
#'
#' @export
comtex_h2 <- function(title, width = 112){

  # Calculate Title Length
  tit_n <- nchar(title) - 1 + 6

  # Calculate Fill Number
  filler <- width - tit_n
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2 - 1
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Entire title
  end <- paste(c(rep("%", fill1), rep(" ", 3),
                 title,
                 rep(" ", 3), rep("%", fill2)),
               collapse = "")

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Copy output to clipboard?")

  if (choice == 1) {

    writeClipboard(end)

  } else {

    return(cat(end))

  }

}
