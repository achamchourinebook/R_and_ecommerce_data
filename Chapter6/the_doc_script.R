# install.packages("udpipe", dependencies = TRUE) 
library(udpipe)

# loading pre-trained
umodel <- udpipe_download_model(language = "english", model_dir="TextProcessingModels")

# we can load the file later
umodel <- udpipe_load_model(file = "TextProcessingModels/english-ewt-ud-2.5-191206.udpipe")

t <- c("I have blue beach ball")

ua <- udpipe_annotate(umodel, t)
ua.df <- as.data.frame(ua)
mutate(ua.df, feats=ifelse(token_id==1 | token_id==2, "", feats) ) %>%
  select(token_id, token, lemma, upos, head_token_id, dep_rel, feats)

t <- c("I have women's mountain bike")

ua <- udpipe_annotate(umodel, t)
ua.df <- as.data.frame(ua)
mutate(ua.df, feats=ifelse(token_id==1 | token_id==2, "", feats) ) %>%
  select(token_id, token, lemma, upos, head_token_id, dep_rel, feats)

t <- c("I have Half-Finger Gloves, L")

ua <- udpipe_annotate(umodel, t)
ua.df <- as.data.frame(ua)
mutate(ua.df, feats=ifelse(token_id==1 | token_id==2, "", feats) ) %>%
  select(token_id, token, lemma, upos, head_token_id, dep_rel, feats)


t <- c("I have half finger gloves, large")

ua <- udpipe_annotate(umodel, t)
ua.df <- as.data.frame(ua)
mutate(ua.df, feats=ifelse(token_id==1 | token_id==2, "", feats) ) %>%
  select(token_id, token, lemma, upos, head_token_id, dep_rel, feats)

###
# text functions

cats.break_hyphenated_words <- function(x)
{
  # x is character vector
  # breaks hyphenated words into separate ones
  
  # regex: lookahead and lookbehind groups
  x <- gsub("(?<=\\w)-(?=\\w)", " ", x, perl=TRUE)
  
  return(x)
}

cats.strip_whitespaces <- function(x)
{
  # x is character vector
  # replaces multiple whitespaces with a single space
  
  # regex: get two or more whitespaces, greedy (all at once)
  x <- gsub("\\s{2,}", " ", x)
  
  return(x)
}

cats.strip_punctuation <- function(x)
{
  # x is character vector
  # replaces multiple punctuation with a single comma
  
  # find punctuation separated by whitespaces, consider them as multiple
  # regex: brackets define a group of chars
  #        one or more of those followed by one or more whitespaces 
  #        and another one or more chars from the group 
  #        all greedy (all at once)
  x <- gsub("[,-]+\\s+[,-]+", ", ", x, perl=TRUE)
  
  x <- gsub("[,-]{2,}", ", ", x, perl=TRUE)
  
  return(x)
}

cats.trim <- function(x)
{
  # x is character vector
  # removes whitespaces and punctuation from the start and the end of a string
  
  # regex: brackets define a group of chars
  #        one or more from the group followed by the end of the string
  x <- gsub("[\\s,-]+$", "", x, perl=TRUE)
  #        one or more from the group preceded by the start of the string
  #        all one or more, all greedy (all at once)
  x <- gsub("^[\\s,-]+", "", x, perl=TRUE)
  
  return(x)
}

cats.remove_numbers <- function(x)
{
  # x is character vector
  
  # regex: brackets define a group of chars
  #        one or more of those, greedy (all at once)
  x <- gsub("[0-9]+", "", x)
  x <- cats.strip_punctuation(x)
  x <- cats.strip_whitespaces(x)  
  x <- cats.trim(x)
  
  return(x)
}

cats.remove_punctuation <- function(x)
{
  # x is character vector
  # removes punctuation completely
  
  # regex: brackets define a group of chars
  #        one or more of those, greedy (all at once)
  x <- gsub("[,-]+", " ", x)
  x <- cats.strip_whitespaces(x)
  x <- cats.trim(x)
  
  return(x)
}  

cats.repl_words <- function(x, words, repls)
{
  # x: vector of expressions in which to replace some words
  # words: vector of words to replace
  # repls: vector of replacements, must have the same order as words
  
  # supported delims are: "," " " "-" and any number
  
  del1 <- "[,\\s-]|[0-9]|^"
  del2 <- "[,\\s-]|[0-9]|$"
  
  for(i in 1:length(words))
  {
    w <- words[i]
    r <- repls[i]
    
    # regex: lookahead and lookbehind groups
    xpr <- paste0("(?<=",del1,")",w,"(?=",del2,")")
    x <- gsub(xpr, r, x, perl=TRUE)
  }
  
  x <- cats.strip_punctuation(x)
  x <- cats.strip_whitespaces(x)
  x <- cats.trim(x)
  
  return(x)
}

cats.get_abbr <- function(x,l=3)
{
  # x vector of poduct/cats names
  # returns words with the length < 3
  
  x <- cats.remove_numbers(x)
  x <- cats.remove_punctuation(x)
  
  x <- unlist(strsplit(x, " ", fixed=TRUE))
  
  return( unique(x[nchar(x)<l]) )
}

source("Modules/dbData.R")
pp <- db.data(vw="vwProductNames", db="AdventureWorks")

pp$product.org <- pp$product 

source("Modules/textFunctions.R")

pp$product <- tolower(pp$product.org)
pp$product <- cats.break_hyphenated_words(pp$product)
pp$product <- cats.remove_numbers(pp$product)
# replace dash with comma
pp$product <- gsub("-", ",", pp$product)

a.abbrs <- cats.get_abbr(pp$product)
a.abbrs

param.abbrs <- data.frame(abbrs=c("l", "m", "s", "xl"), 
                          repls=c("large", "medium", "small", "extra large"))

# generate repl list
a.repls <- a.abbrs
a.repls[a.repls %in% param.abbrs$abbrs] <- param.abbrs$repls
a.repls[nchar(a.repls)<3] <- ""  

pp$product <- cats.repl_words(pp$product, a.abbrs, a.repls)

t <- c("I have cut gem")

ua <- udpipe_annotate(umodel, t)
ua.df <- as.data.frame(ua)
mutate(ua.df, feats=ifelse(token_id==1 | token_id==2, "", feats) ) %>%
  select(token_id, token, lemma, upos, head_token_id, dep_rel, feats)

t <- c("I have a cut gem")

ua <- udpipe_annotate(umodel, t)
ua.df <- as.data.frame(ua)
mutate(ua.df, feats=ifelse(token_id==1 | token_id==2, "", feats) ) %>%
  select(token_id, token, lemma, upos, head_token_id, dep_rel, feats)

pp$product <- paste0("I see a ", pp$product)

uproduct <- udpipe_annotate(umodel, pp$product)
prods <- as.data.frame(uproduct)

docs <- group_by(prods, doc_id) %>% 
  filter(upos=="VERB" & grepl("Tense=Pres", feats)) %>%
  summarise(cnt=n()) %>% filter(cnt>1)

filter(prods, doc_id %in% docs$doc_id) %>% distinct(doc_id, sentence)

if(count(docs)$n > 0)
{
  x <- filter(prods, prods$doc_id %in% docs$doc_id) %>% 
    distinct(doc_id, sentence) %>%
    mutate(nsentence = gsub("I see a", "I see", sentence))
  
  pp$product[pp$product %in% x$sentence] <- x$nsentence
  
  uproduct <- udpipe_annotate(umodel, pp$product)
  prods <- as.data.frame(uproduct)
}

group_by(prods, doc_id) %>% 
  filter(upos=="VERB" & grepl("Tense=Pres", feats)) %>%
  summarise(cnt=n(),sentence=min(sentence)) %>% filter(cnt>1) %>%
  inner_join(pp, by=c("sentence"="product")) %>%
  select(product.org)

# two or more objects
docs <- group_by(prods, doc_id) %>% summarise(cnt=sum(dep_rel=="obj")) %>% filter(cnt>1)
filter(prods, doc_id %in% docs$doc_id) %>% 
  distinct(sentence) %>%
  inner_join(pp, by=c("sentence"="product")) %>%
  select(product.org)

# without object 
x <- filter(prods, dep_rel=="obj") %>% distinct(doc_id)
prods[!(prods$doc_id %in% (x$doc_id)), ] %>% 
  distinct(doc_id, sentence) %>%
  inner_join(pp, by=c("sentence"="product")) %>%
  select(product.org)

# object is not a noun
filter(prods, dep_rel=="obj" & upos!="NOUN") %>% 
  distinct(sentence) %>%
  inner_join(pp, by=c("sentence"="product")) %>%
  select(product.org)

###
prods <- arrange(prods, doc_id, token_id)

docs.count <- length(distinct(prods, doc_id)$doc_id)
l <- vector("list", docs.count)
n <- 1

for(i in 1:length(prods$doc_id))
{
  if(prods[i,"dep_rel"]=="obj")
  {
    docid <- prods[i,"doc_id"]
    sentence <- prods[i,"sentence"]
    token_id <- prods[i, "token_id"] 
    isplural <- grepl("Number=Plur", prods[i, "feats"])
    root <- prods[i,"lemma"]
    troot <- prods[i, "token"]
    words <- c(root)
    twords <- c(troot)
    
    j <- i-1
    while( prods[j,"doc_id"]==docid & 
           prods[j,"token_id"]>2 & 
           prods[j,"head_token_id"]==token_id)
    {
      
      if(!grepl("PronType=Art",prods[j,"feats"]))
      {
        # only if it's not an article
        words <- c(prods[j,"lemma"], words)
        twords <- c(prods[j,"token"], twords)
        isplural <- c(grepl("Number=Plur", prods[j, "feats"]), isplural)
      }
      
      j <- j-1
    }
    
    attr <- NULL
    tattr <- NULL
    
    j <- i+1
    if(prods[j,"dep_rel"]=="punct" & prods[j,"doc_id"]==docid)
    {
      while(!is.na(prods[j,"doc_id"]) & prods[j,"doc_id"]==docid)
      {
        # this may or may not be recognized as related to the object
        if(prods[j,"dep_rel"]!="punct")
        {
          attr <- c(attr, prods[j,"lemma"])
          tattr <- c(tattr, prods[j,"token"])
        }
        
        j <- j+1
      }
    }
    
    l[[n]] <- list("doc_id"=docid, "sentence"=sentence,
                   "root"=root, "words"=words,
                   "twords"=twords, "isplural"=isplural,
                   "attr"=attr, "tattr"=tattr)
    n <- n+1
  }
}

# convert to a dataframe
d <- vector("character",length(l))
s <- vector("character",length(l))
r <- vector("character",length(l))
w <- vector("character",length(l))
tw <- vector("character",length(l))
ip <- vector("character",length(l))
a <- vector("character",length(l))
ta <- vector("character",length(l))
sr <- vector("character",length(l))

for(i in 1:length(l))
{
  d[i] <- l[[i]]$doc_id
  s[i] <- l[[i]]$sentence
  r[i] <- l[[i]]$root
  w[i] <- paste0(l[[i]]$words, collapse=" ")
  sr[i] <- ifelse(length(l[[i]]$words)>1,l[[i]]$words[length(l[[i]]$words)-1],NA)
  tw[i] <- paste0(l[[i]]$twords, collapse=" ")
  ip[i] <- paste0(l[[i]]$isplural, collapse=" ")
  a[i] <- paste0(l[[i]]$attr, collapse=" ")
  ta[i] <- paste0(l[[i]]$tattr, collapse=" ")
}

dtf <- 
  data.frame(doc=d,sentence=s,root=r,sroot=sr,words=w,twords=tw,isplural=ip,attr=a,tattr=ta)

roots <- distinct(dtf, root) %>% arrange(root)
roots$root

filter(pp, grepl(unique(dtf[dtf$root=="mountain","sentence"]),pp$product)) %>% 
  slice_head(n=1) %>%
  select(product.org)

# special case of a missing root >>>
p <- grep("Touring-[0123456789]", pp$product.org)
pp$product.org[p] <- gsub(",", " Bike, ", pp$product.org[p])

p <- grep("Road-[0123456789]", pp$product.org)
pp$product.org[p] <- gsub(",", " Bike, ", pp$product.org[p])

p <- grep("Mountain-[0123456789]", pp$product.org)
pp$product.org[p] <- gsub(",", " Bike, ", pp$product.org[p])
# <<<

# final dataframe
df.cats <- dtf

param.step_back_look_again <- TRUE

if(param.step_back_look_again)
{
  x <- group_by(dtf, root) %>% summarise(n=n()) 
  df.cats <- left_join(dtf,x) %>% arrange(n) 
  
  for(i in 1:length(df.cats$root))
  {
    r <- df.cats[i,"root"]
    sr <- df.cats[i,"sroot"] 
    
    if(!is.na(sr))
    {
      if(length(df.cats[df.cats$root==sr,"n"])>0)
      {
        df.cats[i,"root"] <- sr
        
        w <- unlist(strsplit(df.cats[i,"words"]," ",fixed=TRUE))
        w[c(length(w),length(w)-1)] <- w[c(length(w)-1,length(w))]
        df.cats[i,"words"] <- paste0(w,collapse=" ")
        
        w <- unlist(strsplit(df.cats[i,"twords"]," ",fixed=TRUE))
        w[c(length(w),length(w)-1)] <- w[c(length(w)-1,length(w))]
        df.cats[i,"twords"] <- paste0(w,collapse=" ")
        
        ip <- unlist(strsplit(df.cats[i, "isplural"]," ",fixed=TRUE))
        ip[c(length(ip),length(ip)-1)] <- ip[c(length(ip)-1,length(ip))]
        df.cats[i,"isplural"] <- paste0(ip,collapse=" ")
      }
    }
  }
}

roots <- distinct(df.cats, root) %>% arrange(root)
roots$root

# add labels for presentation purposes, keep the original for future batches
# regex: \\1 keep the first group (beginning of string or whitespace)
#        \\U\\2 uppercase the second group (any word)
df.cats$root.label <- gsub("(^|\\s)(\\w)", "\\1\\U\\2",df.cats$root,perl=TRUE)
df.cats$words.label <- gsub("(^|\\s)(\\w)", "\\1\\U\\2",df.cats$twords,perl=TRUE)
# pluralize
for(i in 1:length(df.cats$root))
{
  ips <- unlist(strsplit(df.cats[i, "isplural"]," ",fixed=TRUE))
  ip <- as.logical(ips[length(ips)])
  if(!ip)
  {
    ss <- substring(df.cats[i,"root.label"],nchar(df.cats[i,"root.label"])-1)
    s2 <- substring(ss,2,2)
    s <- ifelse(ss=="sh" | ss=="ch" | s2=="s" | s2=="x" | s2=="z","es","s")
    
    df.cats[i,"root.label"] <- paste0(df.cats[i,"root.label"],s)
  } else
  {
    # use token instead
    labels <- unlist(strsplit(df.cats[i, "words.label"]," ",fixed=TRUE))
    df.cats[i,"root.label"] <- labels[length(labels)]
  }
}

###
x <- sample(df.cats$sentence, size=30)

filter(df.cats, sentence %in% x) %>% distinct(sentence, root.label) %>%
  inner_join(pp, by=c("sentence"="product")) %>%
  group_by(root.label) %>% slice_head(n=2) %>%
  select(product=product.org, category=root.label) %>%
  arrange(category) %>% print(n=50)
