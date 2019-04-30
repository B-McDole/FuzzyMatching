# Practice Fuzzy Matching
# The purpose of this is to talk a little bit about text mining and 
# trying to match names.  That has been one of the requests that has 
# caused some difficulty in my (brief) time here.  But as I continue
# to work through new techniques I find the process getting easier and a bit
# more streamlined.  I felt like this could possibly be of some help/interest!

library(tm)

#tm is a fantastic library that really reduces the need to write
# some of the more common text altering items by hand.  If you were
# so inclined to write them by hand, what you are usually going to use 
# is something called "regular expressions" so shortened to "regex".
# There is a saying though, "If you have a problem and step one is
# 'write regex' then now you have two problems."  Needless to say,
# regex can be very complicated.
# As a reminder if you do not have tm, you will need to download it first.

# The computation is very very intensive.  As such, we will work
# with a small collection of words.  We will also talk about the theory
# a bit to get comfortable with everything.  Finally, I will show a very
# quick way to keep track of time and periodically check in on computations.

# To start with, we need some words to compare.  I present you with these
# words.

Word.Set.One = c("this", "zebra")
Word.Set.Two = c("this", "This", "THIs", "this ", "Zhis", "th.is", "Ze.bra")

# OK.  Those lists of words are sufficiently silly.  But they are silly
# for a reason!  We want to figure out a few things and talk theory,
# and those words will help do both.

# First, some theory.  There are a lot of ways to determine how similar two
# texts are.  If we are looking at two complete documents then the big ones:
# Cosine similarity (Yes, the trigonometry cosine, and it is beautiful), 
# bag of words (kind of the same thing), Jaccard similarity, and others.

# We, however, are looking to find similarity of individual words.  There
# are a few ways to do so.  Jaro-Winkler distance, Levenshtein distance,
# Needleman-Wunsch algorithm, Smith-Waterman algorithm, and many more,
# can all be used.  We will use, and thus discuss the Levenshtein distance.
# That is way too much markdown, let us do a bit of an experiment 
#to get some code going.  It is boring to read all my writing ;)

# R has a function adist that will perform Levenshtein work for us.
# As a reminder if you type ?adist then the function documentation will
# appear to the right in the help menu.  Do that now so we can discuss it!

?adist

# OK, so the key thing here, we take two arguments, the two lists we want to match.
# We may want to take some other arguments.  costs we can do without.  counts is
# actually also not going to help us so much here either.  
# ignore.case is VERY interesting, as we will see.  

# OK, so a bit of setup.  adist will return a matrix with our first set vertical,
# and our second set horizontal.  Then each element of the matrix is the number of
# changes that needed to happen to make these things match.  A neat exercise,
# what will be in result[1,1]?  It will compare "this" to "this".  Should be 0,
# we agree?  What about result[1,2]?  Let us find out!
  
result = adist(Word.Set.One, Word.Set.Two)

# That is a bit messy.  Let us fix it up some.

row.names(result) = Word.Set.One
colnames(result) = Word.Set.Two

# Remember those from the last thing we did?  We now have some labeled data to allow
# us to see things being compared more clearly.
# Also, if the 'result' tab above does not show the colnames just close the tab
# and then re-open it and they will be there.

# Observations, or otherwise "hot takes" as we call them here:
# result[1,1] beautiful.  Worked exactly as planned.
# Everything else... maybe.  Result[1,2] and Result[1,3] show us that upper/lower
# case matter.  And of course they do!  In fact, if we want to get real
# nerdy and pedantic.  T = 01010100 and t = 01110100 which are of course different!
# Bonus, what are those numbers in base 10?  Those are currently encoded in base 2,
# or binary if you prefer.  But, the moral of the story is, those are different
# to a computer, right?  Right!
# Next note, spacing counts.  Result[1,4] looks frustrating.  We see "this" and 
# "this" because the space is, well, a space.  We will need to account for that!
# As a bonus, yes, space does have a binary representation.  No, we will not worry
# about binary after this.  I just wanted to geek out a bit.  
# Next note, punctuation matters.  I put a "." in the middle of "th.is".  However,
# those are pretty similar right?  So, the distance is 1.  We would just remove
# the period.
# Then of course, zebra is like almost none of the words, and the higher number
# expresses this.  When you have to put all five letters in place you are going
# to get a higher score (higher is bad though!).
# I encourage you to take a second and see if things look and act the way you
# might expect.  Then we will go on to smoothing out problems.

# Moving across the first row, I think we see that if everything were just
# lowercase then we would remove quite a few problems.  This is VERY common
# and something that essentially every language has the capability to do natively.

Word.Set.One = tolower(Word.Set.One) #Quick and easy
Word.Set.Two = tolower(Word.Set.Two) #Same!

result2 = adist(Word.Set.One, Word.Set.Two)
row.names(result2) = Word.Set.One
colnames(result2) = Word.Set.Two

# That fixes quite a bit!  We also need to do some work to get rid of that period.
# While there are some methods (find the punctuation, create substrings?) that
# do not involve writing regex, the most straightforward one is a package!

Word.Set.One = removePunctuation(Word.Set.One)
Word.Set.Two = removePunctuation(Word.Set.Two)

# Before we find the new results, there are two inputs to note.
# First, we can preserve contractions.  Second, we can preserve dashes.
# While neither of those seems groundbreaking, they have been tremendous help in 
# the past.  For example, I was once matching stock-tickers, and that dash was 
# particularly helpful!

removePunctuation(c("can-n.ot", "what's"))
removePunctuation(c("can-n.ot", "what's"), preserve_intra_word_contractions = T)
removePunctuation(c("can-n.ot", "what's"), preserve_intra_word_dashes = T)

# Note the differences!

result3 = adist(Word.Set.One, Word.Set.Two)
row.names(result3) = Word.Set.One
colnames(result3) = Word.Set.Two

# Excellent!  The first row now matches even better.  We even found a match
# for zebra, as odd as that sounds.  We still have that "this " problem.
# That is great, we can now look at a new function, one I use VERY often.
# Leading or trailing blank/white space can make things show up as not equal.
# We can also, with these other functions we use, sometimes accidentally
# cause some whitespace.  Fortunately, there is an answer!

Word.Set.One = trimws(Word.Set.One)
Word.Set.Two = trimws(Word.Set.Two)

# Quick sanity check.  Since there is no more whitespace we should now
# have an exact match at result[4,1].

result4 = adist(Word.Set.One, Word.Set.Two)
row.names(result4) = Word.Set.One
colnames(result4) = Word.Set.Two

# Success!  Things are certainly looking up.  Now we should do a few more 
# interesting things with this.  First off, our sets are small.  They were
# meant for training people, not machines.  Second, with sets this small we can
# quickly and easily parse these rows by hand for the best match.  That is not how
# the data has EVER worked for me.  So we should address that as well.

# First, finding the best match automatically.
# We have two things that can help us.  The first is the "min" function.  This
# will tell us the minimum value for a vector.  Remember the number in the 
# matrix is how many changes had to be made.  So the smaller the better.

test.numbers = c(5,2,3,5,1,7) # Smallest is 1!
min(test.numbers) # Returns 1!
which.min(test.numbers) # Returns 5.  The POSITION of the smallest value (foreshadow)

# So, now we can find both the smallest value, and when it happens.  Excellent!
# I should make one quick note.  which.min only returns the first position of the min,
# not all of the minimums.

# We can use the apply function.  The apply function will take a few arguments,
# first it takes (in this case) a matrix or dataframe.  Second it takes either
# 1 or 2, 1 for rows, 2 for columns.  Third it takes the function you want to apply.
# Last, it takes any additional arguments the function needs.

# If we want to find the smallest number in each column (essentially best match) to
# Word.Set.Two, we can use result, and do some work.

apply(result, 2, min) #we get (0,1,3,1,1,1,2) the smallest elements in each column.
apply(result, 2, which.min) #Should tell us the rows

# No big surprise, with the exception of "Ze.bra", everything was closer to "this".
# If we save the results that tell us the rows and pass it to row names we will
# then get the actual words matching the best.

match.location = apply(result, 2, which.min)
match.names = Word.Set.One[match.location]

# How convenient!  We will see how helpful this is once we get into much larger 
# sets of words.


First.Words = c("This", "is", "Quite inc.", "A", "List", "of Words",
                "Zebra inc", "Silly llc", "on.e", "BONUS")

Second.Words = c("this", "also", "not quite", "an", "zebra", "silly.", "neat",
                 "math", "quite l.l.c")

# Time for some data tidying.

First.Words = tolower(First.Words)
First.Words = removePunctuation(First.Words)
First.Words = trimws(First.Words)
Second.Words = tolower(Second.Words)
Second.Words = removePunctuation(Second.Words)
Second.Words = trimws(Second.Words)

new.result = adist(First.Words, Second.Words)
row.names(new.result) = First.Words
colnames(new.result) = Second.Words

# So, a small issue that comes up here.  llc and inc are both company words.
# Those are commonly confused, and look, they are problematic.
# Quite l.l.c., and Quite inc. are probably the same.  We will need to remove
# words like that.  Often there are other small words we want to remove.  "the", "of",
# words like those.  "Stop Words" they are often called.  We will remove them.

silly.words = c("of", "inc", "llc") # There are some lists available of common words.
First.Words = removeWords(First.Words, words = silly.words)
Second.Words = removeWords(Second.Words, words = silly.words)

# Notice that the silly.words are now gone!  Let us see how that improves our match.

new.result2 = adist(First.Words, Second.Words)
row.names(new.result2) = First.Words
colnames(new.result2) = Second.Words

# Quick problem, the zebras are not matching.  I love that phrase.  Removing the 
# words has introduced white space.  So, we probably want to trimws again, and
# moving forward we probably should trimws last!

First.Words = trimws(First.Words)
Second.Words = trimws(Second.Words)
new.result3 = adist(First.Words, Second.Words)
row.names(new.result3) = First.Words
colnames(new.result3) = Second.Words

# So now we dig out the matches.  We will find the match for each row.
match.location = apply(new.result3, 1, which.min)
match.value = apply(new.result3, 1, min)
match.names = Second.Words[match.location]

# So, now I will link these together for a nice little summary.

final.writeup = rbind(First.Words, match.value, match.names)
final.writeup

# That is a bit much to look at.  We will fix things up.

final.writeup = t(final.writeup)

# Matrix is transposed.  We will now re-label.

final.writeup
row.names(final.writeup) = NULL # Neat thing.  Deleting row names resets your numbering.
colnames(final.writeup) = c("Original.Words", "Match.Score", "Matched.Words")
final.writeup

# So, now we have a halfway decent summary!  Just for me, I am making this a dataframe.
# Now, we may want to do some filtering.  Finding the goodness of match is nice.
# Awkwardly, str(final.writeup) shows us that our middle column is not a number.
# We can change that, and will do so now!

str(final.writeup)
final.writeup$Match.Score = as.numeric(as.character(final.writeup$Match.Score))

final.writeup = data.frame(final.writeup) # So much nicer (to me).

# I can now filter by whatever I want.  If I want a degree of accuracy I can find it.
# Let us filter now by all matches smaller than 2.

filter = which(final.writeup$Match.Score < 2) # Finds only rows where true.
filter2 = final.writeup$Match.Score < 2 # Returns a T/F

# Both will give us what we want.  In this case filter2 is nicer, since we can negate.
# to find the rows with no reasonable match.  We will use that now!

final.writeup$Good.Match[filter2] = "Yes" # Creates a column, puts yes ONLY at filter.
final.writeup$Good.Match[!filter2] = "No" # Fills in the rest!

# As we can see this is not perfect.  Small words that are similar will still
# cause some problems.  But there are few enough that we can check manually.
# This is all still 'fuzzy', but better than manually checking every possible match!

# Now we have a quick way of checking time elapsed.  Clearly the code above was
# quick.  There were not many words and not many lines, etc.  BUT, that is not always
# going to be the case.  You may also be running a lot of code and want periodic
# updates on where you are time-wise (or to make sure it is even still running).

t1 = Sys.time() # This calls the current time.  We save it for reference.
Sys.sleep(5) # This puts a five second delay on our code.  We have a set time.
Sys.time() - t1 # Difference between time at start and current.

# That last output always displays in convenient units (hours/seconds)
# Believe me, knowing when/where your code is in the process is really helpful!

# So that's the gist of it.  There are more complicated things we can do.
# Some of the other things I have used to match include stock-tickers,
# websites, facebook page links, emails, and more.  Each requires finessing
# to make the data usable.  The methods there differ from these of course,
# but in the end I still end up using the processes outlined here on those words
# as well.  
# 
# As always I am happy to talk about it, and would love to follow-up with
# any questions that come up!