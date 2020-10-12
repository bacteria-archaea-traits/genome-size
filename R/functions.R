print("## Loading functions ##", quote = FALSE)

report <- function(message,add_space = FALSE) {
  if(add_space == TRUE) {
    for(l in 1:3) print("", quote = FALSE)
  }
  print(message, quote = FALSE)
}

print("## Done ##", quote = FALSE)