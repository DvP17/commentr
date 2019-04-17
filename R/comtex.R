#' Centered Comments for LaTeX
#'
#' Gives centered comments for LaTeX. Always capitalized.
#'
#' @usage comr(arg1 = "Headline 1")
#'
#' @param arg1 String for title
#'
#' @examples comr("Headline 1")
#'
#' @export
comtex <- function(arg1){
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



commentex2 <- function(arg1){
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
  end <- paste(c(rep("%", fill1), rep(" ", 3), title,
                 rep(" ", 3), rep("%", fill2)),
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
