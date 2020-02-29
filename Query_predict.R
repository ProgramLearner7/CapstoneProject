

## Load data from three data sources (i.e, information from blogs, news and twitter)
library(quanteda)
library(dplyr)
library(stringr)
library(pracma)
blogs = data.frame(source = "blogs", text = readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE))
news = data.frame(source = "news", text = readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE))
twitter = data.frame(source = "tweets", text = readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE))

all = rbind(blogs, news, twitter)


## Sample text data
### Create a subset of data(0.02%) to build the model
set.seed(2638)
blogs.subset = as.character(sample(blogs$text, nrow(blogs)*0.002, replace = TRUE))
news.subset = as.character(sample(news$text, nrow(news)*0.002, replace = TRUE))
twitter.subset = as.character(sample(twitter$text, nrow(twitter)*0.002, replace = TRUE))
all.subset = c(blogs.subset, news.subset, twitter.subset)
saveRDS(all.subset, file = "/Users/cherry/Desktop/Coursera_R/Capstone/all.subset.RData")
all.subset = readRDS(url("https://github.com/ProgramLearner7/CapstoneProject/raw/master/all.subset.RData"))
set.seed(123)
all.subset = sample(all.subset, size = length(all.subset)*0.1, replace = TRUE)

## Count frequencies of unigram, bigram and trigram 
### All the letters were changed to lower case. 
ngram.freq = function(text, ng){
        ngram.dfm = dfm(text, ngram = ng, tolower = TRUE, removePunct = TRUE, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, remove_twitter = TRUE)
        # # keep words occuring >=5 and in >= 3 text
        # tokens.stem.dfm = dfm_trim(tokens.stem.dfm, min_termfreq  = 5, min_docfreq = 3) 
        ngram.dfm.matrix = as.matrix(ngram.dfm)
        freq = colSums(ngram.dfm.matrix)
        ngram.freq = data.frame(ngram = names(freq), freq = freq, row.names = NULL)
        return(ngram.freq) 
}

unigram = ngram.freq(all.subset, 1)
bigram = ngram.freq(all.subset, 2)
trigram = ngram.freq(all.subset, 3)


## The model predicts the third word (wi) after entering two words (wi-2: the first word; wi-1: the second word). For example, in the trigram "case of emergency", the index of "emergency" is wi, "of" is at wi-1 and "case" is at wi-2.
### Set the discount probability of bigram and trigram as 0.75. 
## Find probability of observed trigram starting with the first two words.
obs.trigram = function(pre, trigram, bigram, discount = 0.75) {
        pre = paste("^", pre, sep = "")
        #Find trigrams starting with the first two words
        obs.trigram = trigram[grepl(paste(pre, "_", sep = ""), trigram$ngram), ]
        #Find freqency of wi-2_wi-1 in bigram
        pre.in.bigram = bigram[grepl(paste(pre, "$", sep = ""), bigram$ngram), ]
        if (nrow(obs.trigram) > 0) 
        {
                obs.trigram.prob = (obs.trigram$freq - discount)/pre.in.bigram$freq
                obs.trigram.prob.df = data.frame(ngram = obs.trigram$ngram, prob = obs.trigram.prob, freq = obs.trigram$freq)
                return(obs.trigram.prob.df)
        } else 
        {return(NULL)}
}


## Compare the tail words from observed trigram with unigrams to find the tail word of unobserved trigrams starting with wi-2_wi_1
### If there is no observed trigram, all the unigrams will be returned as the tail word.

unobs.trigram = function(obs.trigram.prob.df, unigram, pre) {
        if (length(nrow(obs.trigram.prob.df)) == 0) 
                return(unigram$ngram)
        
        tail.word.of.obs.trigram = str_split_fixed(obs.trigram.prob.df$ngram, "_", 3)[, 3]
        unobs.tail.word = unigram[!(unigram$ngram %in% tail.word.of.obs.trigram), "ngram"]
        return(unobs.tail.word)
}

## Calculate discounted probability at bigram level (i.e. bigram starting with wi-1)
disc.bigram = function(pre, unigram, bigram, discount = 0.75){
        wi_1 = paste("^", str_split_fixed(pre, pattern = "_", 2)[, 2], sep ="")
        wi_1.bigram = bigram[grepl(paste(wi_1, "_", sep = ""), bigram$ngram), "freq"]
        wi_1.unigram = unigram[grepl(paste(wi_1, "$", sep = ""), unigram$ngram), "freq"]
        if(length(wi_1.bigram) == 0) return(0)
        
        alpha.bi = 1- (sum(wi_1.bigram - discount)/wi_1.unigram)
        return(alpha.bi)
}


## Calculate backed off probabilities for bigrams (i.e probability of unobserved bigrams starting from wi_1)

bigr = function(unobs.trigram.tail, pre, bigram){
        wi_1 = str_split_fixed(pre, "_", 2)[, 2]
        wi_1.bigr.unobstailword = paste(wi_1, unobs.trigram.tail, sep = "_")
        return(wi_1.bigr.unobstailword)
}

obs.bigram = function(unobs.trigram.tail, pre, bigram, unigram, disc.bi){
        bigr = bigr(unobs.trigram.tail, pre, bigram)
        obs.bigr = bigram[bigram$ngram %in% bigr, ]
        wi_1 = paste("^", str_split_fixed(pre, pattern = "_", 2)[, 2], sep ="")
        wi_1.unigram = unigram[grepl(paste(wi_1, "$", sep = ""), unigram$ngram), "freq"]
        obs.bigr.prob = (obs.bigr$freq - disc.bi)/wi_1.unigram
        obs.bigr.prob.df = data.frame(ngram = obs.bigr$ngram, prob = obs.bigr.prob)
        return(obs.bigr.prob.df)
}

unobs.bigram = function(unobs.trigram.tail, pre, obs.bigram, unigram, disc.bi){
        first.word.bigr = str_split_fixed(pre, "_", 2)[,2]
        tail.word.obs.bigram = str_split_fixed(obs.bigram$ngram, "_", 2)[,2]
        if (length(tail.word.obs.bigram) >0) {
                tail.word.unobs.bigr = unobs.trigram.tail[!(unobs.trigram.tail %in% tail.word.obs.bigram)]
                tail.word.unobs.bigr.freq = unigram[unigram$ngram %in% tail.word.unobs.bigr, ]             
        } else {
                tail.word.unobs.bigr.freq = unigram
        }

        denom = sum(tail.word.unobs.bigr.freq$freq)
        unobs.bigram.df = data.frame(ngram = paste(first.word.bigr, tail.word.unobs.bigr.freq$ngram, sep = "_"),
                                     prob = (disc.bi*tail.word.unobs.bigr.freq$freq)/denom)
        return(unobs.bigram.df)
}


## Calculate discount probability at trigram level (i.e. trigram starting with wi_2 and wi_1)
alpha.trigram = function(pre, obs.trigram.prob, bigram, discount = 0.75){
        if(is.null(obs.trigram.prob) == TRUE) return(1)
        wi_2.wi_1.bigram.freq = bigram[bigram$ngram %in% pre, "freq"]
        disc.tri = 1- (sum(obs.trigram.prob$freq - discount)/wi_2.wi_1.bigram.freq)
        return(disc.tri)
}

unobs.trigram.prob = function(pre, all.bigram.start.with.wi_1, disc.tri){
        sum.prob.all.bigram = sum(all.bigram.start.with.wi_1$prob)
        if(sum.prob.all.bigram >0){
                unobs.trigram.prob = (disc.tri*all.bigram.start.with.wi_1$prob)/sum.prob.all.bigram
        }else{unobs.trigram.prob = 0}
        
        fist.word.pre = str_split_fixed(pre, "_", 2)[,1]
        
        unobs.trigram.prob.df = data.frame(ngram = paste(fist.word.pre, all.bigram.start.with.wi_1$ngram, sep = "_"), prob = unobs.trigram.prob)
        return(unobs.trigram.prob.df)
}


## Calculate probabilities of trigram
prediction = function(unobs.trigram.prob, obs.trigram.prob){
        all.trigram = rbind(unobs.trigram.prob, obs.trigram.prob[, c("ngram", "prob")])
        all.trigram = all.trigram[order(-all.trigram$prob), ]
        tail.word.all.trigram = str_split_fixed(all.trigram$ngram, "_", 3)[, 3]
        tail.word.all.trigram.df = data.frame(word = tail.word.all.trigram, prob = all.trigram$prob) 
        return(tail.word.all.trigram.df)
}

## Predict the next word based on given options
prediction.quiz = function(options, pred){
        results = pred[pred$word %in% options, ]
        return(results)
}

predict.results = function(pre, unigram, bigram, trigram, discount.bigram, discount.trigram, options){
        pre = tolower(sub(" ", "_", pre)) 
        obs.trigram.prob = obs.trigram(pre, trigram, bigram)
        unobs.trigram.tail = unobs.trigram(obs.trigram.prob, unigram, pre)
        
        # wi_1.bigram = bigram[grepl(paste("^", str_split_fixed("case_of", pattern = "_", 2)[, 2], "_", sep =""), bigram$ngram),]
        disc.bi = disc.bigram(pre, unigram, bigram, discount.bigram)
        obs.bigram = obs.bigram(unobs.trigram.tail, pre, bigram, unigram, disc.bi)
        unobs.bigram = unobs.bigram(unobs.trigram.tail, pre, obs.bigram, unigram, disc.bi)
        
        all.bigram.start.with.wi_1 = rbind(obs.bigram, unobs.bigram) 
        
        disc.tri = alpha.trigram(pre, obs.trigram.prob, bigram, discount.trigram)
        unobs.trigram = unobs.trigram.prob(pre, all.bigram.start.with.wi_1, disc.tri)
        
        pred = prediction(unobs.trigram, obs.trigram.prob)
        results = prediction.quiz(options, pred)
}


## Calculate perplexity to evaluate model performance
#### https://towardsdatascience.com/perplexity-intuition-and-derivation-105dd481c8f3
all.testing = all[!(all$text %in% all.subset), ]
set.seed(492)
all.testing.sample = as.character(sample(all.testing$text, 200, replace = TRUE))

trigram.testing = ngram.freq(all.testing.sample, 3) %>% filter(freq >=2)

discount.bigram = 0.5
discount.trigram = 0.5
prob.last.word = function(trigram.testing, unigram, bigram, trigram, discount.bigram, discount.trigram, options = unigram$ngram){
        trigram.testing$pre = lapply(trigram.testing$ngram, function(x) substr(x, 1, str_locate_all(x, "_")[[1]][2,1] -1))  
        trigram.testing$last.word = str_split_fixed(trigram.testing$ngram, "_", 3)[,3]
        for(i in 1:nrow(trigram.testing)) {
                prediction = predict.results(trigram.testing$pre[i], unigram, bigram, trigram, discount.bigram, discount.trigram, options)
                prediction$word = as.character(prediction$word)
                trigram.testing$prediction[i] = prediction$word[1]
                if (trigram.testing$last.word[i] %in% prediction$word)  
                {
                        trigram.testing$last.word.prob[i] = prediction[which(prediction$word == trigram.testing$last.word[i]),]$prob}
                if(trigram.testing$last.word.prob[i] == 0) trigram.testing$last.word.prob[i] = NA
        }
        return(trigram.testing)
}

probability.result = prob.last.word(trigram.testing, unigram, bigram, trigram, discount.bigram, discount.trigram)

### Number of matched trigram
sum(probability.result$last.word == probability.result$prediction)
### Number of testing prefix of trigram not in the training data
zero.prob = sum(is.na(probability.result$last.word.prob))
### Perflexity after removing zero probabilities
nthroot(1/(prod(probability.result$last.word.prob, na.rm = T)), nrow(probability.result) - zero.prob)


## Quiz results
results.q1 = predict.results("and I'd", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("die", "give", "sleep", "eat"))
View(results.q1)

results.q2 = predict.results("about his", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("horticultural", "spiritual", "financial", "marital"))
View(results.q2)

results.q3 = predict.results("monkeys this", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("decade", "month", "weekend", "morning"))
View(results.q3)

results.q4 = predict.results("reduce your", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("stress", "sleepiness", "happiness", "hunger"))
View(results.q4)

results.q5 = predict.results("take a", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("picture", "look", "walk", "minute"))
View(results.q5)


results.q6 = predict.results("settle the", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("incident", "case", "account", "matter"))
View(results.q6)

results.q7 = predict.results("in each", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("finger", "toe", "hand", "arm"))
View(results.q7)

results.q8 = predict.results("to the", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("center", "top", "middle", "side"))
View(results.q8)

results.q9 = predict.results("from playing", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("weekly", "inside", "daily", "outside"))
View(results.q9)

results.q10 = predict.results("Adam Sandler", unigram, bigram, trigram, discount.bigram, discount.trigram,  c("movies", "novels", "stories", "pictures"))
View(results.q10)
