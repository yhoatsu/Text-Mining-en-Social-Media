ggplot(vocabulary,aes(y=FREQ))+geom_boxplot()
wordcloud(words = vocabulary$WORD, freq = vocabulary$FREQ, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(6, "Dark2"))


#argentina chile colombia mexico peru spain venezuela

vocabulary_ar <- GenerateVocabulary(path_training, gender="argentina",n, swlang=lang)
vocabulary_ar$var<-rep("Argentina",nrow(vocabulary_ar))
vocabulary_ch <- GenerateVocabulary(path_training, gender="chile",n, swlang=lang)
vocabulary_ch$var<-rep("Chile",nrow(vocabulary_ch))
vocabulary_co <- GenerateVocabulary(path_training, gender="colombia",n, swlang=lang)
vocabulary_co$var<-rep("Colombia",nrow(vocabulary_co))
vocabulary_me <- GenerateVocabulary(path_training, gender="mexico",n, swlang=lang)
vocabulary_me$var<-rep("México",nrow(vocabulary_me))
vocabulary_pe <- GenerateVocabulary(path_training, gender="peru",n, swlang=lang)
vocabulary_pe$var<-rep("Perú",nrow(vocabulary_pe))
vocabulary_sp <- GenerateVocabulary(path_training, gender="spain",n, swlang=lang)
vocabulary_sp$var<-rep("España",nrow(vocabulary_sp))
vocabulary_ve <- GenerateVocabulary(path_training, gender="venezuela",n, swlang=lang)
vocabulary_ve$var<-rep("Venezuela",nrow(vocabulary_ve))

vocabu<-rbind(vocabulary_ar,vocabulary_ch,vocabulary_co,vocabulary_me,vocabulary_pe,vocabulary_sp,vocabulary_ve)

ggplot(vocabu,aes(FREQ,colour=var))+geom_boxplot()+scale_color_brewer(palette="Dark2")
