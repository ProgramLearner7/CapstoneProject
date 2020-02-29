library(shiny)
library(quanteda)
library(dplyr)
library(stringr)
library(wordcloud)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    result <- eventReactive(input$Go, {
    
            validate(
                    need(length(str_split(input$pre, " ")[[1]]) == 2 & str_split(input$pre, " ")[[1]][2] != "", 
                         "Please enter two words in the search box with one space in between and no extra space in start/end of the phrase")
            )
            all.subset = readRDS(url("https://github.com/ProgramLearner7/CapstoneProject/raw/master/all.subset.RData"))
    
    #Build N-gram Model
    ngram.freq = function(text, ng){
            ngram.dfm = dfm(text, ngram = ng, tolower = TRUE, removePunct = TRUE, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, remove_twitter = TRUE)
            # # keep words occuring >=5 and in >= 3 text
            # tokens.stem.dfm = dfm_trim(tokens.stem.dfm, min_termfreq  = 5, min_docfreq = 3) 
            ngram.dfm.matrix = as.matrix(ngram.dfm)
            freq = colSums(ngram.dfm.matrix)
            ngram.freq = data.frame(ngram = names(freq), freq = freq, row.names = NULL)
            return(ngram.freq) 
    }
    
    set.seed(123)
    all.subset = sample(all.subset, size = length(all.subset)*0.1, replace = TRUE)
    unigram = ngram.freq(all.subset, 1)
    bigram = ngram.freq(all.subset, 2)
    trigram = ngram.freq(all.subset, 3)
    
    # The model predicts the third word (wi) after entering two words (wi-2: the first word; wi-1: the second word). 
    # For example, in the trigram "case of emergency", the index of "emergency" is wi, "of" is at wi-1 and "case" is at wi-2.
    obs.trigram = function(pre, trigram, bigram, discount) {
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
    
    unobs.trigram = function(obs.trigram.prob.df, unigram, pre) {
            if (length(nrow(obs.trigram.prob.df)) == 0) 
                    return(unigram$ngram)
            
            tail.word.of.obs.trigram = str_split_fixed(obs.trigram.prob.df$ngram, "_", 3)[, 3]
            unobs.tail.word = unigram[!(unigram$ngram %in% tail.word.of.obs.trigram), "ngram"]
            return(unobs.tail.word)
    }
    
    # Calculate discounted probability at bigram level (i.e. bigram starting with wi-1)
    disc.bigram = function(pre, unigram, bigram, discount){
            wi_1 = paste("^", str_split_fixed(pre, pattern = "_", 2)[, 2], sep ="")
            wi_1.bigram = bigram[grepl(paste(wi_1, "_", sep = ""), bigram$ngram), "freq"]
            wi_1.unigram = unigram[grepl(paste(wi_1, "$", sep = ""), unigram$ngram), "freq"]
            if(length(wi_1.bigram) == 0) return(0)
            
            alpha.bi = 1- (sum(wi_1.bigram - discount)/wi_1.unigram)
            return(alpha.bi)
    }
    
    # Calculate backed off probabilities for bigrams (i.e probability of unobserved bigrams starting from wi_1)
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
    
    # Calculate discount probability at trigram level (i.e. trigram starting with wi_2 and wi_1)
    alpha.trigram = function(pre, obs.trigram.prob, bigram, discount){
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
    
    # Predict the next word
    prediction = function(unobs.trigram.prob, obs.trigram.prob){
            all.trigram = rbind(unobs.trigram.prob, obs.trigram.prob[, c("ngram", "prob")])
            all.trigram = all.trigram[order(-all.trigram$prob), ]
            tail.word.all.trigram = str_split_fixed(all.trigram$ngram, "_", 3)[, 3]
            tail.word.all.trigram.df = data.frame(word = tail.word.all.trigram, prob = all.trigram$prob) 
            return(tail.word.all.trigram.df)
    }
    
    prediction.quiz = function(options, pred){
            results = pred[pred$word %in% options, ]
            return(results)
    }
    
    discount_bigram = 0.75
    discount_trigram = 0.75
    predict.results = function(pre., unigram. = unigram, bigram. = bigram, trigram. = trigram, discount.bigram = discount_bigram, discount.trigram = discount_trigram, options = unigram$ngram){
            pre. = tolower(sub(" ", "_", pre.)) 
            obs.trigram.prob = obs.trigram(pre., trigram., bigram., discount.trigram)
            unobs.trigram.tail = unobs.trigram(obs.trigram.prob, unigram., pre.)
            
            # wi_1.bigram = bigram[grepl(paste("^", str_split_fixed("case_of", pattern = "_", 2)[, 2], "_", sep =""), bigram$ngram),]
            disc.bi = disc.bigram(pre., unigram., bigram., discount.bigram)
            obs.bigram = obs.bigram(unobs.trigram.tail, pre., bigram., unigram., disc.bi)
            unobs.bigram = unobs.bigram(unobs.trigram.tail, pre., obs.bigram, unigram., disc.bi)
            
            all.bigram.start.with.wi_1 = rbind(obs.bigram, unobs.bigram) 
            
            disc.tri = alpha.trigram(pre., obs.trigram.prob, bigram., discount.trigram)
            unobs.trigram = unobs.trigram.prob(pre., all.bigram.start.with.wi_1, disc.tri)
            
            pred = prediction(unobs.trigram, obs.trigram.prob)
    }
    
    result.table = predict.results(input$pre)
    
  })
        
        output$wordcloud <- renderImage({
                
                outfile = tempfile(fileext = ".png")
                
                png(outfile, width = 300, height = 300)
                result.table = result()
                wordcloud(result.table$word, freq = result.table$prob, max.words = 150, random.order = FALSE, 
                          random.color = TRUE, colors=brewer.pal(8, "Dark2"))
                dev.off()
                list(src = outfile,
                     contentType = 'image/png',
                     width = 300,
                     height = 300)
                }, deleteFile = T)
        
        output$table = renderTable({
                result.table = result()
                result.table[1:6,]
        })
        
        output$predict.word = renderText({
                top6 = result()
                as.character(top6$word[1])
        })
})

