# RandomTextGenerator

This task was made within the course "Introduction to Functional Programming in OCaml" on www.fun-mooc.fr

### Description from the course:

The goal of this project is to synthesize natural language sentences using information extracted from an existing text corpus.

For this, given a text corpus as input, we will first compute the frequency of all sequences of two words in the original text; then we will use this information to produce new sentences by randomly collating these sequences with the same frequencies.

This method is known under the term of Markov chain. From the input text, we compute a transition table that associates to each word the list of words that may appear after it, with their relative frequencies. 

### Description for parts

[Part A: A First Draft](Project_Part_A.ml)

Our first goal will be to build such a table and generate sentences from it, quick and dirty style, using lists and their predefined operators.

[Part B: Performance Improvements](Project_Part_B.ml)

Now, we want to use more efficient data structures, so that we can take larger inputs and build bigger transition tables.

In this exercise, we will use hash tables, predefined in OCaml in the Hashtbl module. Used correctly, hash table provide both fast insertion and extraction. 

[Part C: Quality Improvements](Project_Part_C.ml)

If we want to generate sentences from larger corpuses, such as the ones of the ebooks_corpus given in the prelude, we cannot just ignore the punctuation. We also want to generate text using not only the beginning of the original text, but the start of any sentence in the text. 

Now, we will drastically improve the results by matching sequences of more than two words. We will thus update the format of our tables again, and use the following `ptable`

So let's say we want to identify sequences of N words in the text. The `prefix_length` field contains N−1. The `table` field associates each list of N−1 words from the text with the distribution of its possible successors. 

([Prject_Part_C_Ch_2.ml](Project_Part_C_Ch_2.ml) contains function `merge_ptables` that combines several tables together)
