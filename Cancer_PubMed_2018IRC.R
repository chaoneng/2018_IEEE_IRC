#######
#Charles C.N. Wang 
#2018 IEEE IRC
#Asia University
#######

########
#input xml from Pubmed 
########

extract_xml <- function(theFile) {
  library(XML)
  newData <- xmlParse(theFile)
  
  records <- getNodeSet(newData, "//PubmedArticle")
  pmid <- xpathSApply(newData,"//MedlineCitation/PMID", xmlValue)
  authLast <- lapply(records, xpathSApply, ".//Author/LastName", xmlValue)
  
  ## affiliations <- lapply(records, xpathSApply, ".//Author/AffiliationInfo/Affiliation", xmlValue)
  ## affiliations[sapply(affiliations, is.list)] <- NA
  ## affiliations <- sapply(affiliations, paste, collapse = "|")
  
  year <- lapply(records, xpathSApply, ".//PubDate/Year", xmlValue) 
  year[sapply(year, is.list)] <- NA
  year <- unlist(year)
  
  articletitle <- lapply(records, xpathSApply, ".//ArticleTitle", xmlValue) 
  articletitle[sapply(articletitle, is.list)] <- NA
  articletitle <- unlist(articletitle)
  
  journal <- lapply(records, xpathSApply, ".//ISOAbbreviation", xmlValue) 
  journal[sapply(journal, is.list)] <- NA
  journal <- unlist(journal)
  
  abstract <- lapply(records, xpathSApply, ".//Abstract/AbstractText", xmlValue)
  abstract[sapply(abstract, is.list)] <- NA
  abstract <- sapply(abstract, paste, collapse = "|")
  
  theDF <- data.frame(pmid, year, articletitle, journal, abstract,stringsAsFactors = FALSE)
  return(theDF)
}

paperdf = extract_xml("/Users/charleswang/Desktop/demo/pubmed_result.xml")
paperdf$journal

######
#Journal Conunt
######

library(RISmed)
#journal<-result$pubmedArticle
journal <- paperdf$journal

cancer_journal_count <- as.data.frame(table(journal))
cancer_journal_count_top25 <- cancer_journal_count[order(-cancer_journal_count[,2]),][1:25,]
#write.table(cancer_journal_count,"D:/jounsl_non_count")
####

#####
journal_names <- paste(cancer_journal_count_top25$journal,"[jo]",sep="")

total_journal <- NULL
for (i in journal_names){
  perjournal <- EUtilsSummary(i, type='esearch', db='pubmed',mindate=2013, maxdate=2017)
  total_journal[i] <- QueryCount(perjournal)
}

journal_cancer_total <- cbind(cancer_journal_count_top25,total_journal)
names(journal_cancer_total) <- c("journal","cancer_publications","Total_publications")
journal_cancer_total$cancer_publications_normalized <- journal_cancer_total$cancer_publications / journal_cancer_total$Total_publications

write.table(journal_cancer_total,"D:/cancer_publications_per_journal.txt",quote=F,sep="\t",row.names=F)

pubs_per_journal <- read.table("D:/cancer_publications_per_journal.txt",header = T,sep="\t")


#plot
ggplot(pubs_per_journal,aes(journal, cancer_publications,fill=journal)) + geom_bar(stat="identity")+
  coord_flip()+
  theme(legend.position="none")

ggplot(pubs_per_journal ,aes(journal, cancer_publications_normalized,fill=journal)) + geom_bar(stat="identity")+
  coord_flip()+
  theme(legend.position="none")

######
#cancer of type
######
library(stringr)
#ab = Abstract
ab =paperdf$abstract
#ab=result$pubmedAbstractText

br=str_count(ab,"breast cancer")
lu=str_count(ab,"lung cancer")
ba=str_count(ab,"brain cancer")
co=str_count(ab,"colon cancer")
bl=str_count(ab,"bladder cancer")
ce=str_count(ab,"cervical cancer")
re=str_count(ab,"rectal cancer")
li=str_count(ab,"liver cancer")
bla=str_count(ab,"bladder cancer")
kid=str_count(ab,"Kidney cancer")
pa=str_count(ab,"pancreatic cancer")
th=str_count(ab,"thyroid cancer")
pr=str_count(ab,"prostate cancer")
sk=str_count(ab,"skin cancer")
vu=str_count(ab,"vulvar cancer")
va=str_count(ab,"vaginal cancer")
na=str_count(ab,"nasopharyngeal cancer")
ov=str_count(ab,"ovarian cancer")
pe=str_count(ab,"penile cancer")
pro=str_count(ab,"prostate cancer")
bon=str_count(ab,"bone cancer")
bd=str_count(ab,"bile Duct cancer")
ana=str_count(ab,"anal cancer")
blad=str_count(ab,"bladder cancer")
eye=str_count(ab,"eye cacner")

cbin=cbind(eye,blad,br,lu,ba,co,bl,ce,re,li,bla,kid,pa,th,pr,sk,vu,va,na,ov,pe,pro,bon,bd,ana)
#count(cbin, vars = c("eye", "blad","br","lu","ba","co","bl","ce","re","li","bla","kid","pa","th","pr","sk","vu","va","na","ov","pe","pro","bon","bd","ana"))
cancer_freq<- colSums(cbin)
cancer_freq<-as.data.frame(cancer_freq)
cancer <-  c("eye","blad","br","lu","ba","co","bl","ce","re","li","bla","kid","pa","th","pr","sk","vu","va","na","ov","pe","pro","bon","bd","ana")

count_cancer = cbind(cancer, cancer_freq)

###plot
ggplot(count_cancer,aes(cancer,cancer_freq,fill= cancer)) + geom_bar(stat="identity")+
  coord_flip()+
  theme(legend.position="none")


######
##define same word
#####

synonyms <- list(
  list(word="cancer", syns=c("cancers"))
)
synonyms2<- list(
  list(word="patient", syns=c("patients"))
)
replaceSynonyms <- content_transformer(function(x, syn=NULL) { 
  Reduce(function(a,b) {
    gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
})

######
# tm remove
#####

##define same word
synonyms <- list(
  list(word="cancer", syns=c("cancers"))
)
synonyms2<- list(
  list(word="patient", syns=c("patients"))
)
replaceSynonyms <- content_transformer(function(x, syn=NULL) { 
  Reduce(function(a,b) {
    gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
})

####
library(tm)
docs <- Corpus(VectorSource(ab)) 

clean_corpus <- function(corpus){
  # Eliminate extra white spaces
  corpus <- tm_map(corpus, stripWhitespace)
  # Remove punctuations
  corpus <- tm_map(corpus, removePunctuation)
  # Convert the text to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove english common stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"thou"))
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  # specify your stopwords as a character vector
  corpus <- tm_map(corpus, removeWords, c("the","cell","used","\n")) 
  
  ##synonyms
  corpus <- tm_map(corpus, replaceSynonyms, synonyms)
  corpus <- tm_map(corpus, replaceSynonyms, synonyms2)
  
  return(corpus)
}

docs<-clean_corpus(docs)
dtm <- DocumentTermMatrix(docs) 

findFreqTerms(dtm,10)
findAssocs(dtm, "colitis", 0.5)

inspect(removeSparseTerms(dtm, 0.6))

tdm <- TermDocumentMatrix(docs)   
freq <- colSums(as.matrix(dtm)) 
length(freq) 
head(table(freq), 20) 

###wordcloud for freq
library(wordcloud2)
set.seed(142)  
w2w=data.frame("name" =names(freq),"freg"=freq)
wordcloud2(w2w)

###Creating a Barplot???
tdm<-as.matrix(tdm)
term_frequency<-rowSums(tdm)
term_frequency[1:10]
barplot(term_frequency[1:10],col="lightgreen",las=2)

###define compute year for freq
####

library(qdap)
myFunc<-function(argument){
  articles1<-data.frame('Abstract'=paperdf$abstract, 'Year'=paperdf$year)
  abstracts1<-articles1[which(articles1$Year==argument),]
  abstracts1<-data.frame(abstracts1)
  abstractsOnly<-as.character(abstracts1$Abstract)
  abstractsOnly<-paste(abstractsOnly, sep="", collapse="")
  abstractsOnly<-as.vector(abstractsOnly)
  abstractsOnly<-strip(abstractsOnly)
  stsp<-rm_stopwords(abstractsOnly, stopwords = qdapDictionaries::Top100Words)
  ord<-as.data.frame(table(stsp))
  ord<-ord[order(ord$Freq, decreasing=TRUE),]
  head(ord,100) #??????
}
###
years_2017<-myFunc(2017)
#oSeven<-myFunc(2007)
all<-cbind(years_2017)
names(all)<-c("2017","freq")

######
#plot wordcloud
######

###wordcloud for freq
wordcloud2(all)
#wordcloud2(all, figPath = "/Users/charleswang/Desktop/DNA.png",color = "black")

###plot year 
paperdf %>%
  group_by(year) %>%
  count() %>%
  ggplot(aes(year, n)) +
  geom_point() +
  geom_line() +
  labs(title = "Pubmed articles with search terms `data science` & `population health` \n2015-2016", hjust = 0.5,
       y = "Articles")

paperdf %>%
  group_by(year) %>%
  count() %>%
  filter(year > 2013) %>%
  ggplot(aes(year, n)) +
  geom_point() +
  geom_line() +
  labs(title = "Pubmed articles with search terms `data science` & `population health` \n2015-2016", hjust = 0.5,
       y = "Articles")


#####
# yers
######


#######
#word2Vec and LDA process
######

#mixseg = worker(type = "mix",user = "D:/user.txt")
#str2=segment(str, mixseg )
library(text2vec)
docs=as.character(docs)
it = itoken(docs, progressbar = FALSE)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5L)
vectorizer = vocab_vectorizer(vocab)

tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")

######
#LSA model
#######
lda_model = LDA$new(n_topics = 5, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 1000, 
                                          convergence_tol = 0.001, n_check_convergence = 25, 
                                          progressbar = FALSE)

###plot boxplot of topic model
barplot(doc_topic_distr[1, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1), 
        names.arg = 1:ncol(doc_topic_distr))

lda_model$get_top_words(n = 10, topic_number = c(1L, 3L, 5L), lambda = 1)
lda_model$plot()
######
#LDA model2
######
library(topicmodels)
library(tidytext)
library(tidyr)
#row_total=slam::row_sums(dtm2, na.rm = T)
row_total = apply(dtm, 1, sum)
dtm.new = dtm[row_total>0,]


desc_lda <- LDA(dtm.new, k = 10, control = list(seed = 42))
tidy_lda <- tidy(desc_lda)


top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

######
#word2vec
######
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
wv_main = fit_transform(tcm, glove, n_iter = 20)
wv_context = glove$components

word_vectors = wv_main + t(wv_context)

###Woer2Vec Query
berlin = word_vectors["cancer", , drop = FALSE] - 
  word_vectors["man", , drop = FALSE] + 
  word_vectors["woman", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

#######
#compute n-gram with tidy
#######
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(grid)
library(ggraph)

d <- data_frame(paperdf$abstract,txt = paperdf$abstract)
austen_bigrams <- d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)
austen_bigrams %>%
  count(ngram, sort = TRUE)
bigrams_separated <- austen_bigrams %>%
  separate(ngram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n > 6) %>%
  graph_from_data_frame()

set.seed(100)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.02, 'inches')) +
  geom_node_point(color = "lightblue", size = 1) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
  #####END####
