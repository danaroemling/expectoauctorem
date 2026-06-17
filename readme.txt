This repository has all code that was used in the analysis of the "Expecto Auctorem" paper. 

The data for this work is the Darmstadt Fanfiction Corpus:
Glawion, Anastasia, und Thomas Weitin. „Darmstadt Fanfiction Corpus 1.0 (Fanfiktion.de, 2020–2023)". TU Darmstadt, 2024. https://doi.org/10.48328/TUDATALIB-1452 

Initial steps in terms of cleaning, understanding dimensions etc:
- author_numbers.R: Provides an initial assessment of the dimensions of the corpus
- subcorpus_creation.R: Gets a specific fandom and genre and extracts the respective authors/texts
- subcorpus_cleaning.R: Sets thresholds for author inclusion and creates a directory only with authors that match the criteria
- train_test_split.R: Creates a train and test directory with 70% training data per author

Second analysis step:
- 15k_cleaning.R: Sets the threshold for each author to have at least 15,000 words of training data
- lemma_and_POS.R: Lemmatises and POS-tags each chunk in the training data
- checking_tagging.R: Does what is on the tin; export seeded sample to inspect
- how_are_spells_tagged.R: Using known spells extracts seeded sample to inspect
- pos_ngrams.R: Gets the POS 1-10-grams for each chunk and each author (raw and relative)
- check_ngrams.R: Checks output is reasonable
- pos_profiles.R: Gives a profile of the top POS-grams for each author
- cliffs_delta.R: Calculates Cliff's Delta (one-vs-rest pairwise, capped at 50 chunks per author for compute reasons)
- looking_at_cliffs.R: Analysis helper with visualisation
- MACD.R: Aggregates the temp files from cliffs_delta.R
- author_consistency.R: for each feature, measured how consistent an author's usage of that n-gram is across their own chunks
- filtered_feature_list.R: gets the features that are reliable AA features (stable, frequent and with discriminatory power)
