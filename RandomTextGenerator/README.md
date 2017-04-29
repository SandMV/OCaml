This task was made within the course "Introduction to Functional Programming in OCaml" on /www.fun-mooc.fr

The goal of this project is to synthesize natural language sentences using information extracted from an existing text corpus.

For this, given a text corpus as input, we will first compute the frequency of all sequences of two words in the original text; then we will use this information to produce new sentences by randomly collating these sequences with the same frequencies.

This method is known under the term of Markov chain. From the input text, we compute a transition table that associates to each word the list of words that may appear after it, with their relative frequencies. 
