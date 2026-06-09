This repository has all code that was used in the analysis of the "Expecto Auctorem" paper. 

The data for this work is the Darmstadt Fanfiction Corpus:
Glawion, Anastasia, und Thomas Weitin. „Darmstadt Fanfiction Corpus 1.0 (Fanfiktion.de, 2020–2023)". TU Darmstadt, 2024. https://doi.org/10.48328/TUDATALIB-1452 

Initial steps in terms of cleaning, understanding dimensions etc:
- author_numbers.R: Provides an initial assessment of the dimensions of the corpus
- subcorpus_creation.R: Gets a specific fandom and genre and extracts the respective authors/texts
- subcorpus_cleaning.R: Sets thresholds for author inclusion and creates a directory only with authors that match the criteria
- train_test_split.R: Creates a train and test directory with 70% training data per author


