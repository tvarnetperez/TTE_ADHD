# Enclose every element in `character_vector` in "mi( . . .)". For use in bf formulas
enclose_mi <- function(character_vector){
  sapply(character_vector, function(x) paste0("mi(", x, ")")) |> unname()
}

# Filter `character_vector` to only values that contain `number`
select_number <- function(character_vector, number){
  if (is.character(number)) {
    grep(character_vector, pattern = number, value = TRUE)
  } 
  
  if (is.numeric(number)) {
    grep(character_vector, pattern = as.character(number), value = TRUE)
  } 
}

select_domain <- function(character_vector, domain = c("MA", "EN", "RE")){
  grep(character_vector, pattern = domain, value = TRUE)
}

# Remove dots and underscores
strip <- function(character_vector){
  gsub("[._]", "", x = character_vector)
}

# Wrapper for how many missing
sum_NA <- function(x){sum(is.na(x))}

  
# All score variables, remove dots and underscores
stripped_test_vars <- strip(c("p.EN_5"
                              , "p.MA_5"
                              , "p.RE_5"
                              , "p.EN_8"
                              , "p.MA_8"
                              , "p.RE_8" )
)

# Enclose them in mi( )
posttest_imputation_missing_vars <- enclose_mi(stripped_test_vars) |> unname()  
  
# This function will output the missing terms of the specific `grade`
# enclosed in mi() except for the one defined in `domain`
build_eta_mi_domain <- function(character_vector = posttest_imputation_missing_vars,
                                domain, grade){
  stopifnot("Domain should be a single character string,
            either 'MA', 'RE' or 'EN'" =
              is.character(domain) &
              length(domain) == 1 &
              domain %in% c("MA", "EN", "RE"))
  # Filter first by year
  character_vector <- select_number(character_vector, number = grade)
  # Select all that don't match the domain
  tmp <- grep(character_vector,
              pattern = domain,
              invert = TRUE,
              value = TRUE)
  # Collapse into single string of additive terms
  paste(tmp, collapse = " + ")
}