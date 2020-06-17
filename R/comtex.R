#' Centered Comments for LaTeX
#'
#' Gives centered comments for LaTeX. Always capitalized.
#'
#' @usage comr(mode = "Headline 1")
#'
#' @param string String for title
#' @param mode Formatting
#'
#' @examples comr("Headline 1")
#'
#' @export
comtex <- function(string, mode = 1){

  # Calculate Title Length
  if(mode == 1) {
    title <- nchar(string) + nchar(string) - 1 + 6
  } else if(mode == 2) {
    title <- nchar(string) - 1 + 6
  }

  # Calculate Fill Number
  filler <- 112 - title
  if((filler %% 2) == 0) {
    fill1 <- filler / 2
    fill2 <- filler / 2
  } else {
    fill1 <- (filler - 1) / 2
    fill2 <- (filler - 1) / 2 + 1
  }

  # Capitalize
  string <- toupper(string)


  # Entire String
  if(mode == 1) {

    # Spaces Between Letters
    title <- sub("\\s+$", "", gsub('(.{1})', '\\1 ', string))

    end <- paste(c(rep("%", 112), "\n", rep("%", fill1), rep(" ", 3), title,
                 rep(" ", 3), rep("%", fill2), "\n", rep("%", 112)),
               collapse = "")
  } else if(mode == 2) {
    end <- paste(c(rep("%", fill1), rep(" ", 3), string,
                   rep(" ", 3), rep("%", fill2)),
                 collapse = "")
  }

  # Write to Clipboard
  choice <- menu(c("Yes", "No"),
                 title="Do you want to copy into your clipboard?")

  if (choice == 1) {

    writeClipboard(end)

  } else {

    return(cat(end))

  }

}



