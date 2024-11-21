
# String Manipulation and Custom Pig Latin Assignment

## Overview
This assignment includes two exercises focusing on string manipulation in R. The first exercise analyzes word frequencies in Jane Austen's "Pride and Prejudice" to identify the most common words, excluding stopwords. The second exercise involves creating a custom Pig Latin converter function that modifies English words according to specific rules.

## Exercise 1: Word Frequency Analysis
- **Objective**: Analyze "Pride and Prejudice" to plot the most common words, removing "stop words."
- **Tools Used**: `janeaustenr`, `tidyverse`, `tidytext`, `stopwords`

## Exercise 2: Custom Pig Latin Converter
- **Objective**: Create a function that converts words into a customized version of Pig Latin.
- **Description**: The function rearranges and modifies words based on whether they start with vowels or consonants and adds a suffix "bee" to each word.
- **Tools Used**: `stringr`

## Getting Started
To run the analysis and view the results:
1. Install R and RStudio.
2. Install the required packages using R commands:
   ```R
   install.packages("janeaustenr")
   install.packages("tidyverse")
   install.packages("tidytext")
   install.packages("stopwords")
   install.packages("stringr")
   ```
3. Run the R scripts provided in the R Markdown file.

## Files
- `assignment.Rmd`: Contains the detailed code for both exercises.
- `assignment.md`: Markdown file generated from the R Markdown file.

## Results
- Exercise 1 results in a plot of the top 20 most common words in "Pride and Prejudice" after removing stopwords.
- Exercise 2 demonstrates the custom Pig Latin converter with various examples and passes all designed tests.
