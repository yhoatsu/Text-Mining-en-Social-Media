###################### Funci?n GenerateBoWngrams  ##################################
# Modificaci?n de la funci?n GenerateBoW para usar ngramas en lugar las palabras m?s frecuentes

# Preparing parameters
n <- 100
lang <- "es"
path_training <- "G:/GoogleDrive/Master Big Data/Text Mining en Social Media/pan-ap17-bigdata/training"  	# Your training path
path_test <- "G:/GoogleDrive/Master Big Data/Text Mining en Social Media/pan-ap17-bigdata/test"							# Your test path
k <- 3
r <- 1

#argentina chile colombia mexico peru spain venezuela

vocabulary <- GenerateVocabulary(path_training, gender="venezuela",n, swlang=lang)
c<-tokenize_ngrams(vocabulary,n=2)
lista<-unlist(c)
ngramas<-table(lista)
ngramas<-as.data.frame(ngramas)
names(ngramas)<-c("ngrama","freq")
ngramas_ord<-ngramas[order(ngramas$freq,decreasing = T),]

vocabulary_ngram<-ngramas_ord[1:n,]
names(vocabulary_ngram)<-c("WORD","FREQ")

bow_training_gender <- GenerateBoWngrams(path_training, vocabulary_ngram, class="gender")
##############
ggplot(vocabulary,aes(y=FREQ))+geom_boxplot()
##############
GenerateBoWngrams <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    
    line <- author
    #Función que toqueniza en dos ngramas todos los twitts del autor actual
    c<-tokenize_ngrams(txtdata,n=2)
    #Se hace el unlist y table par realizar el conteo de apariciones de cada ngrama
    lista<-unlist(c)
    ngramas<-table(lista)
    #Se convierte a data frame para facilitar su ordenación
    ngramas<-as.data.frame(ngramas)
    names(ngramas)<-c("WORD","FREQ")
    #Se ordenan los ngramas por orden de frecuencia decreciente
    ngramas_ord<-ngramas[order(ngramas$FREQ,decreasing = T),]
    #Tenemos de forma análoga al data frame creado para bolsa de palabras solo que con n-gramas
    freq<-ngramas_ord[1:n,]

    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      }
      line <- paste(line, ",", thefreq, sep="")
    }
    
    # Concatenating the corresponding class: variety or gender
    if (class=="variety") {
      line <- paste(variety, ",", line, sep="")
    } else {
      line <- paste(gender, ",", line, sep="")
    }
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}