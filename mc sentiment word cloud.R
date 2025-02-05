############################################################
# malt cross sentiment word cloud from TripAdvisor reviews
############################################################

mc <- read.csv("maltcross1.csv")
colnames(mc) <- "comments"
mc.pol <- polarity(mc$comments)
mc.pol$polarity <- scale(mc.pol$all$polarity)

mc.pol$polarity <- na.omit(mc.pol$all[,3])
# write to csv, delete NaN and read back in

df <- as.data.frame(mc.pol$all)
df <- df[!is.na(df$polarity),]

View(df)
colnames(mc.pol)
head(mc.pol)

mc.pol.df <- as.data.frame(mc.pol)
ggplot(df, aes(x=polarity))+
  theme_gdocs()+
  geom_histogram(binwidth=0.25, fill="darkred", colour="grey60", size=0.2)+
  geom_density(size=0.75)

mc.pos.comments <- subset(mc$comments,mc$polarity>0)
mc.neg.comments <- subset(mc$comments,mc$polarity<0)

mc.pos.terms <- paste(mc.pos.comments,collapse=" ")
mc.neg.terms <- paste(mc.neg.comments,collapse=" ")
mc.all.terms <- c(mc.pos.terms, mc.neg.terms)
mc.all.corpus <- Corpus(VectorSource(mc.all.terms))

mc.all.tdm <- TermDocumentMatrix(mc.all.corpus,
                                 control=list(weighting=weightTfIdf))

mc.all.tdm.m <- as.matrix(mc.all.tdm)
colnames(mc.all.tdm.m) <- c('positive','negative')

comparison.cloud(mc.all.tdm.m, max.words=100, colours=c('darkgreen','darkred'))


# use original files which gives messier results

mc.pos.terms <- unlist(df$pos.words)
mc.neg.terms <- unlist(df$neg.words)
mc.pos.terms <- paste(df$pos.words,collapse=" ")
mc.neg.terms <- paste(df$neg.words,collapse=" ")

# both
mc.all.terms <- c(mc.pos.terms, mc.neg.terms)
mc.all.corpus <- Corpus(VectorSource(mc.all.terms))
mc.all.tdm <- TermDocumentMatrix(mc.all.corpus,
                                 control=list(weighting=weightTfIdf))
mc.all.tdm.m <- as.matrix(mc.all.tdm)
colnames(mc.all.tdm.m) <- c('positive','negative')
comparison.cloud(mc.all.tdm.m, max.words=500, colours=c('darkgreen','darkred'))


# OR use hand-edited files for some pre-processing

mc.pos.terms <- read.csv("mcp1.csv")
mc.neg.terms <- read.csv("mcn1.csv")
mc.pos.terms <- unlist(mc.pos.terms)
mc.neg.terms <- unlist(mc.neg.terms)

# both
mc.all.terms <- c(mc.pos.terms, mc.neg.terms)
mc.all.corpus <- Corpus(VectorSource(mc.all.terms))
mc.all.tdm <- TermDocumentMatrix(mc.all.corpus,
                                 control=list(weighting=weightTfIdf))
mc.all.tdm.m <- as.matrix(mc.all.tdm)
colnames(mc.all.tdm.m) <- c('positive','negative')
comparison.cloud(mc.all.tdm.m, max.words=500, colours=c('darkgreen','darkred'))
