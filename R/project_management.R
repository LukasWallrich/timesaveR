#' Set up analysis project folder and script files
#'
#' This sets up simple folder structure and template files for an analysis project. 
#' 
#' The template includes two helpers worth highlighting. The **`take_note()`**
#' function allows to add character strings to a note file that is saved at
#' the end of each script. Usage should follow the first example: 
#' `take_note("Note created:", timestamp(quiet = TRUE))`. Note that this modifies
#' `notes` in place, so the result should not be assigned.
#' 
#' Then there is the **`0_run_all.R`** file, which runs all analysis scripts in order
#' and documents when this last happened. 
#'
#' @param folder Root folder of the project to be set up. Defaults to here::here()
#' @param analyses Character vector of analysis steps. R files will be set up in order.
#' @param pipeline_name Name of folder for outputs from each analysis step
#' @param code_folder Logical. Should code files be placed in /code sub-folder?
#' Otherwise, are placed in root folder
#' @param standard_packages Character vector of packages to be loaded at start of each analysis file.
#' @param github_packages Character vector of packages to be loaded and installed from Github if needed at start of each analysis file.
#' @source The structure is based on https://towardsdatascience.com/how-to-keep-your-research-projects-organized-part-1-folder-structure-10bd56034d3a, with some simplifications and additions.
#' @export

setup_analysis_project <- function(folder = here::here(), analyses = c("data_prep", "analyses", "presentation"), 
                                   pipeline_name = "outputs", code_folder = FALSE, 
                                   standard_packages = c("magrittr", "here", "dplyr"), 
                                   github_packages = NULL) {
  
  pipeline_folder <- paste0("3_", pipeline_name)
  folders <- paste0(folder, "/", c("0_data", "1_tools", (if (code_folder) "2_code" else NULL), pipeline_folder))

  purrr::map(folders, function(x) {
    if (!dir.exists(x)) {
      dir.create(x)
    }
  })

  files <- paste0(seq_along(analyses), "_", analyses, ".R")
  if (code_folder) files <- paste0("2_code/", files)

  # Adds packages to code template
  code_template <- glue::glue(code_template)

  for (i in seq_along(analyses)) {
    filename <- analyses[i]
    previous_name <- paste0(analyses[i - 1], "")
    # Adds file names to code_template
    code <- glue::glue(code_template)
    writeLines(code, file.path(folder, files[i]))
  }

  writeLines(glue::glue(management_functions_file), file.path(folder, "1_tools", "management_functions.R"))

  writeLines(glue::glue(run_all_file), file.path(folder, (if (code_folder) "2_code" else ""), "0_run_all.R"))
}


code_template <- ('

# ------------
# Introduction
# ------------

# !! Describe file purpose !!

NAME <- "{{filename}}"

# ------------
# Sources
# ------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load({paste(standard_packages, collapse=", ")})
{if(is.null(github_packages)) "" else "pacman::p_load_gh("}\\
{if(is.null(github_packages)) "" else paste0(paste0("\'", github_packages, "\'"), collapse=", ")}\\
{if(is.null(github_packages)) "" else ")"}

source(here("1_tools/management_functions.R"))

#Set up pipeline folder if missing
pipeline <- create_pipeline_dir(NAME)
datadir <- "0_data"
pipelinedir <- "3_{pipeline_name}"

#df <- read_!!!(here(datadir, "!!!"))
#df <- read_!!!(here(pipelinedir, "{{previous_name}}", "!!!"))

notes <- character()
take_note("Note created:", timestamp(quiet = TRUE))


# ------------
# STEP 1
# ------------


# ------------
# Save outputs
# ------------

# readr::write_rds(df, here(pipeline, "XXX.RDS"))

writeLines(notes, here(pipeline, "notes.txt"))
                  ')

management_functions_file <- ("
  create_pipeline_dir <- function (NAME) {{
  pipeline <- here('3_{pipeline_name}', NAME)
  if (!dir.exists(pipeline)) {{
    dir.create(pipeline)
}}

  stringr::str_replace(stringr::str_replace(pipeline, here(), ''), '^/', '')
  }}

take_note <- function(...) {{
  x <- list(...) 
  if (length(x)>1) {{
    x <- unlist(x)
    cat(paste(x, collapse = ' '))
    notes <<- c(notes, paste(x, collapse = ' '))
  } else {{
    x <- x[[1]]
    if ('data.frame' %in% class(x)) {{
      res <- capture.output(print.data.frame(data.frame(round_df(x, 3)), row.names = FALSE, right = FALSE))
    }} else {{
      res <- capture.output(print(x))
    }}
    print(res)
    notes <<- c(notes, paste(res, collapse = '\n'))
  }}
  invisible(x)
}}

                              ")


run_all_file <- ('
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, purrr)

files <- list.files(here({if(code_folder) "2_code" else ""}), pattern = "\\.R$")[-1]

map(here(files), source)

notes <- character()

notes <- c(notes, "Last complete run:", timestamp())

writeLines(notes, here("last_complete_run.txt"))

                 ')

#' @title Export package-specific R snippets
#'
#' @description `add_package_snippets` copies all (missing) snippet definitions
#'   in 'inst/rstudio/r.snippets' and 'rmd.snippets' (if not empty) to the RStudio user snippet location.
#'
#' @return boolean invisible(FALSE) if nothing was added, invisible(TRUE) if snipped definitions were added
#' @export
#'
#' @examples
#' \dontrun{
#' add_package_snippets()
#' }
#' @source https://stackoverflow.com/a/62223103/10581449
#' @keywords internal

add_package_snippets <- function() {
  added <- FALSE

  # if not on RStudio or RStudioServer exit
  if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    warning("Code snippets are only implemented in RStudio - so cannot do anything here.")
    return(NULL)
  }

  # Name of files containing snippet code to copy
  pckgSnippetsFiles <- c("r.snippets", "rmd.snippets")

  # Name of files to copy into. Order has to be the same
  # as in 'pckgSnippetsFiles'
  rstudioSnippetsFiles <- c("r.snippets", "markdown.snippets")

  # Path to directory for RStudios user files depends on OS and RStudio version

  if (rstudioapi::versionInfo()$version < "1.3") {
    rstudioSnippetsPathBase <- file.path(path.expand("~"), ".R", "snippets")
  } else {
    if (.Platform$OS.type == "windows") {
      rstudioSnippetsPathBase <- file.path(Sys.getenv("APPDATA"), "RStudio", "snippets")
    } else {
      rstudioSnippetsPathBase <- file.path(path.expand("~"), ".config/rstudio", "snippets")
    }
  }

  # Read each file in pckgSnippetsFiles and add its contents
  #
  for (i in seq_along(pckgSnippetsFiles)) {

    # Try to get template, if template is not found skip it
    #
    pckgSnippetsFilesPath <- system.file("rstudio", pckgSnippetsFiles[i], package = "timesaveR")
    if (pckgSnippetsFilesPath == "") {
      next()
    }

    # load package snippets definitions
    pckgSnippetsFileContent <- readLines(pckgSnippetsFilesPath, warn = FALSE)

    # Extract names of package snippets
    pckgSnippetsFileDefinitions <- pckgSnippetsFileContent[grepl("^snippet (.*)", pckgSnippetsFileContent)]


    # Construct path for destination file
    rstudioSnippetsFilePath <- file.path(rstudioSnippetsPathBase, rstudioSnippetsFiles[i])

    # If targeted RStudios user file does not exist, raise error (otherwise we would 'remove')
    # the default snippets from the 'user file'
    if (!file.exists(rstudioSnippetsFilePath)) {
      stop(
        "'", rstudioSnippetsFilePath, "' does not exist yet\n.",
        "Use RStudio -> Tools -> Global Options -> Code -> Edit Snippets\n",
        "to initalize the user defined snippets file by adding any (dummy) snippet\n"
      )
    }

    # Extract 'names' of already existing snippets
    rstudioSnippetsFileContent <- readLines(rstudioSnippetsFilePath, warn = FALSE)
    rstudioSnippetDefinitions <- rstudioSnippetsFileContent[grepl("^snippet (.*)", rstudioSnippetsFileContent)]

    # replace two spaces with tab, ONLY at beginning of string
    pckgSnippetsFileContentSanitized <- gsub("(?:^ {2})|\\G {2}|\\G\t", "\t", pckgSnippetsFileContent, perl = TRUE)

    # find definitions appearing in packageSnippets but not in rstudioSnippets
    # if no snippets are missing go to next file

    snippetsToCopy <- setdiff(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    snippetsNotToCopy <- intersect(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    if (length(snippetsToCopy) == 0) {
      cat(paste0(
        "\n(", pckgSnippetsFiles[i], ": Following snippets will NOT be added because there is already a snippet with that name: ",
        paste0(snippetsNotToCopy, collapse = ", "), ")\n"
      ))
      next()
    }

    # Inform user about changes, ask to confirm action
    #
    if (interactive()) {
      cat(paste0(
        "You are about to add the following ", length(snippetsToCopy),
        " snippets to '", rstudioSnippetsFilePath, "':\n",
        paste0(paste0("-", snippetsToCopy), collapse = "\n")
      ))
      if (length(snippetsNotToCopy) > 0) {
        cat(paste0(
          "\n(The following snippets will NOT be added because there is already a snippet with that name:\n",
          paste0(snippetsNotToCopy, collapse = ", "), ")"
        ))
      }
      answer <- readline(prompt = "Do you want to proceed (y/n): ")
      if (substr(answer, 1, 1) == "n") {
        next()
      }
    }

    # Create list of line numbers where snippet definitions start
    # This list is used to determine the end of each definition block
    #
    allPckgSnippetDefinitonStarts <- grep("^snippet .*", pckgSnippetsFileContentSanitized)

    for (s in snippetsToCopy) {
      startLine <- grep(paste0("^", s, ".*"), pckgSnippetsFileContentSanitized)

      # Find last line of snippet definition:
      # First find start of next definition and return
      # previous line number or last line if already in last definition

      endLine <- allPckgSnippetDefinitonStarts[allPckgSnippetDefinitonStarts > startLine][1] - 1
      if (is.na(endLine)) {
        endLine <- length(pckgSnippetsFileContentSanitized)
      }

      snippetText <- paste0(pckgSnippetsFileContentSanitized[startLine:endLine], collapse = "\n")

      # Make sure there is at least one empty line between entries

      if (utils::tail(readLines(rstudioSnippetsFilePath, warn = FALSE), n = 1) != "") {
        snippetText <- paste0("\n", snippetText)
      }

      # Append snippet block, print message
      #
      cat(paste0(snippetText, "\n"), file = rstudioSnippetsFilePath, append = TRUE)
      added <- TRUE
    }
  }

  if (added) {
    cat("Restart RStudio to use new snippets")
  }

  return(invisible(added))
}

# Copy Rproj filepath to clipboard - e.g., to set up local links
rproj_to_clip <- function() {
  here::here(list.files(here::here(), pattern = "[.]Rproj$")) %>% clipr::write_clip()
}

#' @title Save ggplot-graph and show in folder
#'
#' @description This wraps `ggsave` and opens the folder where the graph was saved in a Shell.
#' From there, it can easily be dragged and dropped into the application where you want to use it.
#' It also changes the default units from in to cm, and defaults to saving temporary png files.
#'
#' @param filename File name with path. If not provided, only a temporary file is saved
#' @param units Unit for width and height, if provided. Defaults to "cm", can also be "in" or "mm"
#' @inheritParams ggplot2::ggsave
#' @inheritDotParams ggplot2::ggsave
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(mpg)) + geom_histogram()
#' ggsave_show(here::here("mtcars.pdf"))
#' }
#' @source https://stackoverflow.com/a/12135823/10581449

ggsave_show <- function(filename = NULL, ..., device = NULL, units = "cm") {
  if(is.null(filename)) {
    filename <- tempfile(Sys.time() %>% 
                           {paste0("0-plot-", format(., "%H"), "-", format(., "%M"), "-")}, 
                         fileext = ".png")
  }
  
  ggplot2::ggsave(filename, units = units, device = device, ...)
  if (.Platform["OS.type"] == "windows") {
    shell.exec(dirname(filename))
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dirname(filename)))
  }
}
