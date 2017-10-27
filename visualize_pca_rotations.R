visualize_pca_rotations = function (pca.obj, r = NULL, group = NULL, weights = NULL, alpha =NULL, 
          low = "#FFFFFF", mid='#f7f7f7',high = "#9E0142") 
{
  # if (is.list(svd.obj) & all(names(svd.obj) %in% c("u", "d", 
  #                                                  "v"))) {
  #   print("Your input data is treated as a SVD output, with u, d, v corresponding to left singular vector, singular values, and right singular vectors, respectively.")
  # }
  # else {
  #   print("Your input data is treated as (typically, right) singular vectors. For example, it should be svd.obj$v  from a SVD output.")
  #   svd.obj = list(v = svd.obj)
  # }
  svd.obj= pca.obj$rotation
  if (is.null(dimnames(svd.obj))) {
    colnames(svd.obj) = paste0("V", 1:ncol(svd.obj))
    rownames(svd.obj) = paste0("ID ", 1:ncol(svd.obj))
  }
  if(is.null(alpha)){alpha=0}
  if (is.null(r)) 
    r = ncol(svd.obj)
  if (r > 30) 
    print("It may not be good to visualize too many singular vectors or principal components at one.")
  if (is.null(group)) 
    group = rep(1, nrow(svd.obj))
  if (!is.null(weights)) {
    if (weights == "sv") {
      print("Singular values are used as weights.")
      weights = svd.obj$d[1:r]
    }
    else if (length(weights) != r) {
      stop("The length of weights must equal r.")
    }
  }
  if (!is.null(weights)) {
    mv = as.data.frame(weights * svd.obj$v[, 1:r])
  }else {
    mv = as.data.frame(svd.obj[, 1:r])
  }
  mv$rnames <- rownames(mv)
  mv = melt(as.data.frame(mv), id.vars = "rnames", value.name = "value", 
            variable.name = "cnames")
  g = ggplot(mv, aes_string(x = "rnames", y = "variable")) + 
    labs(x = "", y = "") + scale_x_discrete(expand = c(0, 
                                                       0)) + scale_y_discrete(expand = c(0, 0)) + theme(legend.position = "bottom", 
                                                      axis.ticks = element_blank(), axis.text.x = element_text(angle = 90, 
                                                     hjust = 0, colour = "grey50")) + geom_tile(aes_string(fill = "value"), 
                                                                    colour = "white") + scale_fill_gradient2("", low = low,mid=mid , high = high)
  
  return(g)
}