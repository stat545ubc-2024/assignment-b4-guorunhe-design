Exercises on String Manipulation and Custom Pig Latin
================

# Exercise 1: Word Frequency Analysis in “Pride and Prejudice”

## Introduction

This exercise involves analyzing a text from Jane Austen’s “Pride and
Prejudice” to identify the most common words, excluding stopwords.

## Load Necessary Packages

References:

- janeaustenr Package: Accessing Jane Austen’s books.
  <https://github.com/juliasilge/janeaustenr>

- stopwords Package: For a predefined list of stop words.
  <https://cran.r-project.org/web/packages/stopwords/readme/README.html>

``` r
# Load the libraries
library(janeaustenr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidytext)
library(stopwords)
```

## Load and Prepare the Text

``` r
# Load "Pride and Prejudice" text
book_text <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  select(text)

# Display the first few lines
head(book_text)
```

    ## # A tibble: 6 × 1
    ##   text                 
    ##   <chr>                
    ## 1 "PRIDE AND PREJUDICE"
    ## 2 ""                   
    ## 3 "By Jane Austen"     
    ## 4 ""                   
    ## 5 ""                   
    ## 6 ""

## Tokenize Text and Remove Stop Words

``` r
# Define stop words using the stopwords package (English)
stop_words_custom <- stopwords::stopwords("en")

# Tokenize the text and remove stop words
tidy_words <- book_text %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words_custom) %>%
  filter(str_detect(word, "^[a-z]+$"))  # Keep only words with letters

# View the tidy words
head(tidy_words)
```

    ## # A tibble: 6 × 1
    ##   word     
    ##   <chr>    
    ## 1 pride    
    ## 2 prejudice
    ## 3 jane     
    ## 4 austen   
    ## 5 chapter  
    ## 6 truth

## Count Word Frequencies

``` r
# Count the most common words
word_counts <- tidy_words %>%
  count(word, sort = TRUE)

# View the top 20 most common words
head(word_counts, 20)
```

    ## # A tibble: 20 × 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 mr          785
    ##  2 elizabeth   597
    ##  3 said        401
    ##  4 darcy       373
    ##  5 mrs         343
    ##  6 much        326
    ##  7 must        305
    ##  8 bennet      294
    ##  9 miss        283
    ## 10 jane        264
    ## 11 one         263
    ## 12 bingley     257
    ## 13 know        236
    ## 14 though      226
    ## 15 well        224
    ## 16 never       218
    ## 17 soon        216
    ## 18 think       211
    ## 19 can         210
    ## 20 now         204

## Plot the Most Common Words

``` r
# Select the top 20 words
top_words <- word_counts %>%
  top_n(20, n) %>%
  arrange(desc(n))

# Plot
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Common Words in 'Pride and Prejudice'",
       x = "Words",
       y = "Frequency") +
  theme_minimal()
```

![](assignment_files/figure-gfm/plot-1.png)<!-- -->

# Exercise 2: Custom Pig Latin Converter

## Function Definition

``` r
# Load necessary library for string manipulation
library(stringr)

#' Convert a Word to Custom Pig Latin
#'
#' This function converts English words to a customized version of Pig Latin by applying specific rearrangement and addition rules.
#'
#' Rearrangement Rules:
#' - Consonant Start: If the word begins with a consonant or a consonant cluster, move the initial consonant(s) to the end of the word.
#' - Vowel Start: If the word begins with a vowel, move the last consonant in the word to the beginning.
#' - No Vowels: If the word contains no vowels, leave it unchanged in the rearrangement phase.
#'
#' Addition Rule:
#' - Append "bee" to the end of the rearranged word.
#'
#' @param word A single word (character string) to be converted. Must contain only alphabetic characters.
#' @return A character string representing the word in customized Pig Latin.
#' @examples
#' custom_pig_latin("hello")    # "ellohbee"
#' custom_pig_latin("apple")    # "lappebee"
#' custom_pig_latin("string")   # "ingstrbee"
#' custom_pig_latin("orange")   # "goranebee"
#' custom_pig_latin("rhythm")   # "rhythmbee"
#' @export
custom_pig_latin <- function(word) {
  # Input validation
  if (!is.character(word)) {
    stop("Input must be a character string.")
  }
  
  if (length(word) != 1) {
    stop("Input must be a single word.")
  }
  
  if (!str_detect(word, "^[A-Za-z]+$")) {
    stop("Input must contain only alphabetic characters.")
  }
  
  # Define vowels
  vowels <- c("a", "e", "i", "o", "u")
  
  # Convert word to lowercase for processing
  lower_word <- tolower(word)
  
  # Function to identify the initial consonant cluster at the start
  get_initial_consonant_cluster <- function(w) {
    first_vowel_pos <- str_locate(w, "[aeiou]")[1, "start"]
    if (is.na(first_vowel_pos)) {
      # No vowels, entire word is consonant cluster
      return(w)
    } else if (first_vowel_pos == 1) {
      # Starts with vowel
      return("")
    } else {
      # Return the initial consonant cluster
      return(str_sub(w, 1, first_vowel_pos - 1))
    }
  }
  
  # Function to identify the last consonant in the word
  get_last_consonant <- function(w) {
    # Find all consonants in the word
    consonants <- str_extract_all(w, "[bcdfghjklmnpqrstvwxyz]")[[1]]
    if (length(consonants) == 0) {
      return("")
    } else {
      # Find the last consonant
      last_consonant <- tail(consonants, n = 1)
      return(last_consonant)
    }
  }
  
  # Function to replace the last occurrence of a pattern in a string
  replace_last_occurrence <- function(string, pattern, replacement) {
    # Create a regex pattern that matches the last occurrence of the pattern
    # Uses a negative lookahead to ensure it's the last occurrence
    regex_pattern <- paste0("(", pattern, ")(?!.*", pattern, ")")
    
    # Perform the replacement
    str_replace(string, regex_pattern, replacement)
  }
  
  # Check if the first letter is a vowel
  first_letter <- str_sub(lower_word, 1, 1)
  
  if (first_letter %in% vowels) {
    # Vowel start: move the last consonant to the beginning
    last_consonant <- get_last_consonant(lower_word)
    if (last_consonant == "") {
      # No consonants, leave the word as is
      rearranged_word <- lower_word
    } else {
      # Remove the last consonant from its position using the replacement logic
      rearranged_word <- replace_last_occurrence(lower_word, fixed(last_consonant), "")
      # Prepend the last consonant
      rearranged_word <- str_c(last_consonant, rearranged_word)
    }
  } else {
    # Consonant start: move the initial consonant cluster to the end
    initial_consonant_cluster <- get_initial_consonant_cluster(lower_word)
    if (initial_consonant_cluster == "") {
      rearranged_word <- lower_word
    } else {
      # Remove the initial consonant cluster
      rearranged_word <- str_replace(lower_word, fixed(initial_consonant_cluster), "")
      # Append the initial consonant cluster to the end
      rearranged_word <- str_c(rearranged_word, initial_consonant_cluster)
    }
  }
  
  # Addition component: append "bee"
  pig_latin_word <- str_c(rearranged_word, "bee")
  
  # Preserve original casing: if the original word starts with an uppercase letter, capitalize the first letter
  if (str_detect(word, "^[A-Z]")) {
    pig_latin_word <- str_to_title(pig_latin_word)
  }
  
  return(pig_latin_word)
}
```

## Examples

``` r
# Example 1: Word starts with a consonant
custom_pig_latin("hello")   # Expected Output: "ellohbee"
```

    ## [1] "ellohbee"

``` r
# Example 2: Word starts with a vowel
custom_pig_latin("apple")   # Expected Output: "lappebee"
```

    ## [1] "lappebee"

``` r
# Example 3: Word with consonant cluster
custom_pig_latin("string")  # Expected Output: "ingstrbee"
```

    ## [1] "ingstrbee"

``` r
# Example 4: Word starts with a vowel and has multiple consonants
custom_pig_latin("orange")  # Expected Output: "goranebee"
```

    ## [1] "goranebee"

``` r
# Example 5: Word with no vowels
custom_pig_latin("rhythm")  # Expected Output: "rhythmbee"
```

    ## [1] "rhythmbee"

``` r
# Example 6: Capitalized word
custom_pig_latin("Hello")   # Expected Output: "Ellohbee"
```

    ## [1] "Ellohbee"

## Tests

``` r
# Load necessary libraries
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
test_that("custom_pig_latin returns correct Pig Latin conversion", {
  # Test 1: Word starts with a consonant
  expect_equal(custom_pig_latin("hello"), "ellohbee")
  
  # Test 2: Word starts with a vowel
  expect_equal(custom_pig_latin("apple"), "lappebee")
  
  # Test 3: Word with consonant cluster
  expect_equal(custom_pig_latin("string"), "ingstrbee")
  
  # Test 4: Word with no vowels
  expect_equal(custom_pig_latin("rhythm"), "rhythmbee")
  
  # Test 5: Capitalized word
  expect_equal(custom_pig_latin("Hello"), "Ellohbee")
})
```

    ## Test passed 🌈

``` r
test_that("custom_pig_latin output ends with 'bee'", {
  # Example words
  words <- c("hello", "apple", "string", "orange", "rhythm", "Umbrella")
  
  # Apply the function
  transformed_words <- sapply(words, custom_pig_latin)
  
  # Check that each transformed word ends with "bee"
  expect_true(all(str_ends(transformed_words, "bee")))
})
```

    ## Test passed 🥇

``` r
test_that("custom_pig_latin handles invalid inputs appropriately", {
  # Test with numeric input
  expect_error(custom_pig_latin(123), "Input must be a character string.")
  
  # Test with multiple words
  expect_error(custom_pig_latin(c("hello", "world")), "Input must be a single word.")
  
  # Test with non-alphabetic characters
  expect_error(custom_pig_latin("hello123"), "Input must contain only alphabetic characters.")
  
  expect_error(custom_pig_latin("hello-world"), "Input must contain only alphabetic characters.")
  
  # Test with empty string
  expect_error(custom_pig_latin(""), "Input must contain only alphabetic characters.")
})
```

    ## Test passed 😀
