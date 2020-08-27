find_word_template <-
  function(template_name = "word-styles-reference-01.docx") {

    path <- system.file("templates", template_name,
                        package = "mal")

    if (identical(path, "")) {
      stop("Could not find Word template.")
    }
    path
  }
