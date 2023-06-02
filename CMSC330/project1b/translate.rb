class Translator
  @@wordHash
  @@grammarHash
  def getWordHash
    return @wordHash  
  end
  def getGrammarHash
    return @grammarHash 
  end

  def initialize(words_file, grammar_file)
    grammarHash = Hash.new();
    wordHash = Hash.new();
  
    opened_word_file = File.open(words_file)
    opened_grammar_file = File.open(grammar_file)
    #initial grammar check
    checkGrammar = /^(?:([A-Z](?:[a-z]|[0-9])*): [A-Z]{3}(?:{\d})?(?:, [A-Z]{3}(?:{\d})?)*)$/
    #initial word checker
    checkWord = /^(?:[a-z]|-)*, [A-Z]{3}, (?:([A-Z](?:[a-z]|[0-9])*):(?:[a-z]|-)+, )*(?:([A-Z](?:[a-z]|[0-9])*):(?:[a-z]|-)+)$/
    #Language for Grammar
    grammarLang = /([A-Z](?:[a-z]|[0-9])*): /
    #Rule for Grammar
    grammarRule = /([A-Z]{3}(?:{\d})?)/
    #holds the current word and pos
    wordAndPos = /((?:[a-z]|-)*), ([A-Z]{3})/
    #language and its translated word
    langAndT_Word = /(?:, ([A-Z](?:[a-z]|[0-9])*):((?:[a-z]|-)+){0,})/
    #expand {/d} into muitple indexes
    expandPOS = /([A-Z]{3})(?:{([0-9])})/
    normPos = /([A-Z]{3})/
  
    line = opened_grammar_file.gets
    #this will worry about grammar
    while line
      if checkGrammar =~ line
        if grammarLang =~ line
          languageName = $1.to_s
          grammarHash[languageName] = Array.new
          if grammarRule =~ line
            grammarHash[languageName] = line.scan(grammarRule)
            newGrammar = Array.new
            j = 0
            for i in 0..grammarHash[languageName].length()
                if expandPOS =~ grammarHash[languageName][i].to_s
                    for k in 0..$2.to_i-1
                        newGrammar[j] = $1
                        j+=1
                    end
                else
                    normPos =~ grammarHash[languageName][i].to_s
                    newGrammar[j] = $1
                    j+=1
                end
            end
          end
        end
        grammarHash[languageName] = newGrammar.compact
      end
      line = opened_grammar_file.gets
    end
    opened_grammar_file.close
    #worry about word
    #implement here
    line = opened_word_file.gets
    while line
      if checkWord =~ line
        if wordAndPos =~ line
          thePos = $2.to_s
          theWord = $1.to_s
          if wordHash.key?(thePos) == false
              wordHash[thePos] = Hash.new(theWord)
          end
          wordHash[thePos][theWord] = Hash.new()
          if langAndT_Word =~ line
            langAndWordArray = line.scan(langAndT_Word)
            for i in 0..langAndWordArray.length()-1
                wordHash[thePos][theWord][langAndWordArray[i][0].to_s] = langAndWordArray[i][1].to_s
            end
            #put in English Translations
            wordHash[thePos][theWord]["English"] = theWord
          end
        end
      end
      line = opened_word_file.gets
    end
    opened_word_file.close
    @wordHash = wordHash
    @grammarHash = grammarHash
  end
  
  # part 1
  def updateLexicon(inputfile)
    #initial word checker
    checkWord = /^(?:[a-z]|-)*, [A-Z]{3}, (?:([A-Z](?:[a-z]|[0-9])*):(?:[a-z]|-)+, )*(?:([A-Z](?:[a-z]|[0-9])*):(?:[a-z]|-)+)$/
    #holds the current word and pos
    wordAndPos = /((?:[a-z]|-)*), ([A-Z]{3})/
    #language and its translated word
    langAndT_Word = /(?:, ([A-Z](?:[a-z]|[0-9])*):((?:[a-z]|-)+){0,})/
  
    opened_word_file = File.open(inputfile)
    line = opened_word_file.gets
    while line
      if checkWord =~ line
        if wordAndPos =~ line
          updateWord = $1.to_s
          updatePos = $2.to_s
          #search for wordHash with current updateWord
          #if the updateWord does not exist, create new Hashes for
          #the word and the pos
          #array of the line, this should exist for passing initial regex checking
          langAndTransArray = line.scan(langAndT_Word)
          #check to see if words exist, if not create hash for position
          if @wordHash.key?(updatePos) == false
            @wordHash[updatePos] = Hash.new
          end
          if @wordHash[updatePos].key?(updateWord) == false
            @wordHash[updatePos][updateWord] = Hash.new()
          end
          for i in 0..langAndTransArray.length()-1
              lang =langAndTransArray[i][0]
              word =langAndTransArray[i][1]
              @wordHash[updatePos][updateWord][lang] = word
          end
          @wordHash[updatePos][updateWord]["English"] = word
        end
      end
      line = opened_word_file.gets
    end
    opened_word_file.close

  end
  
  def updateGrammar(inputfile)
    #initial grammar check
    checkGrammar = /^(?:([A-Z](?:[a-z]|[0-9])*): [A-Z]{3}(?:{\d})?(?:, [A-Z]{3}(?:{\d})?)*)$/
    #Language for Grammar
    grammarLang = /([A-Z](?:[a-z]|[0-9])*): /
    #Rule for Grammar
    grammarRule = /([A-Z]{3}(?:{\d})?)/
    #language and its translated word
    langAndT_Word = /(?:, ([A-Z](?:[a-z]|[0-9])*):((?:[a-z]|-)+){0,})/
    #expand {/d} into muitple indexes
    expandPOS = /([A-Z]{3})(?:{([0-9])})/
    normPos = /([A-Z]{3})/
  
  opened_grammar_file = File.open(inputfile)
  line = opened_grammar_file.gets
    #this will worry about grammar
    while line
      if checkGrammar =~ line
        if grammarLang =~ line
          languageName = $1.to_s
          if @grammarHash.key?(languageName) == false
            @grammarHash[languageName] = Array.new
          end
          if grammarRule =~ line
          @grammarHash[languageName] = line.scan(grammarRule)
          newGrammar = Array.new
          j = 0
          for i in 0..@grammarHash[languageName].length()
              if expandPOS =~ @grammarHash[languageName][i].to_s
                  for k in 0..$2.to_i-1
                      newGrammar[j] = $1
                      j+=1
                  end
              else
                  normPos =~ @grammarHash[languageName][i].to_s
                  newGrammar[j] = $1
                  j+=1
              end
          end
        end
      end
      @grammarHash[languageName] = newGrammar.compact
    end
    line = opened_grammar_file.gets
  end
  end
  def generateSentence(language, struct)
      langStruc = Array.new
      if (struct.class == String) && (@grammarHash.key?(struct) == false)
          return nil
      elsif struct.class == Array
          langStruc = struct
      else
          langStruc = @grammarHash[struct]
      end
      #now generate the sentence in Spanish using english words
      newSentence = Array.new
      for i in 0..langStruc.length()-1
          #if there is no pos, return nil
          if @wordHash.key?(langStruc[i]) == false
              return nil
          end
          arrayOfT_Words = Array.new()
          count = 0;
          @wordHash[langStruc[i]].each do |word, translations|
              #if there is no translated word of that pos, return nil
              if translations[language] == false
                  return nil
              end
              if translations[language] != nil
                  arrayOfT_Words[count] = translations[language]
                  count += 1
              end
          end
          newSentence[i] = arrayOfT_Words.sample()
          if newSentence.include?(nil)
            return nil
          end
      end
      return newSentence.join(" ")
  end
  
  def checkGrammar(sentence, language)
    theLine = sentence.split(" ")
    if theLine.length != @grammarHash[language].length
      return false
    end
    #see if the pos matches
    count = 0
    for i in 0..@grammarHash[language].length-1
      for j in @wordHash[@grammarHash[language][i]].keys
        if @wordHash[@grammarHash[language][i]][j][language] == theLine[i]
          count += 1
          break
        end
      end
    end
    if count == theLine.length
      return true
    end
    return false
  end

 def changeGrammar(sentence, struct1, struct2)
    firstStruct = Array.new
    changeToStruct = Array.new
    #worry about first struct
    if struct1.class == String
      firstStruct = @grammarHash[struct1]
    elsif struct1.class == Array
      firstStruct = struct1
    else
      return nil
    end
    #worry for second struct
    if struct2.class == String
      changeToStruct = @grammarHash[struct2]
    elsif struct2.class == Array
      changeToStruct = struct2
    else
      return nil
    end
    theLine = sentence.split(" ")
    #count and find
    count = 0
    for i in 0..theLine.length()-1
      getWordHash.each do |pos, posHash|
          posHash.each do |word, wordHash|
            wordHash.each do |language, translatedWord|
              if theLine[i] == translatedWord
                count+=1
              end
            end
          end
      end
    end
    if count != theLine.length
      return nil
    end

  
    #create a hash for first Struct => sentences
    orderedArray = Array.new(theLine.length())
    #tempHash keeps track of the amount of words i am using
    temp2D = []
       for i in 0..theLine.length()-1
         temp2D[i] = [firstStruct[i], theLine[i]]
      end
   #not working
   for i in 0..changeToStruct.length()-1
     for j in 0..temp2D.length()-1
      if temp2D[j][0] == changeToStruct[i]
        orderedArray[i] = temp2D[j][1]
        temp2D[j][0] = nil
        break
      end
     end
   end
   
   #now you have the hash, remove as you place everything in
   #puts orderedArray.to_s
   if orderedArray.include?(nil)
     return nil
   end
  return orderedArray.join(" ")
 end
  
  # part 3
  def changeLanguage(sentence, language1, language2)
    givenLine = sentence.split(" ")
    translatedArray = Array.new
    #loop through everything
    for i in 0..givenLine.length()-1
      getWordHash.each do |pos, posHash|
          posHash.each do |word, wordHash|
            wordHash.each do |language, translatedWord|
              if givenLine[i] == translatedWord
                translatedArray[i] = @wordHash[pos][word][language2]
              end
            end
          end
      end
    end
    #now check
    for i in 0..translatedArray.length-1
      if translatedArray[i] == nil
        return nil
      end
    end
    return translatedArray.join(" ")
  end 


  def translate(sentence, language1, language2)
    changedGrammar = changeGrammar(sentence, language1, language2)
      if changedGrammar == nil
        return nil
      end
      return changeLanguage(changedGrammar, language1, language2)
  end

  def translate(sentence, language1, language2)
    changedGrammar = changeGrammar(sentence, language1, language2)
    if changedGrammar == nil
      return nil
    end
    return changeLanguage(changedGrammar, language1, language2)
  end
end