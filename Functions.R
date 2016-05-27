# random functions for use

  multi_grep_character <- function(find, inthis){ #returns location of multiple "find' elements in the vector 'inthis'
    if(class(inthis)!= "character"){break("Error: in this must be a character vector")}
    return(unlist(lapply(1:length(find), function(x) {grep(find[x],inthis)}   )))
  }

  outersect <- function(x, y) {
    sort(c(x[!x%in%y],
           y[!y%in%x]))
  }

