scientific_10_v2 <- function(x) {
  v1 = gsub("e", " %*% 10^", scales::scientific_format()(x))

  v2 = str_replace_all(v1,"1 %\\*% ","")
  v3 = str_replace_all(v2,"\\+","")
  #browser()

  parse(text=v3)
}
