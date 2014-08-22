codeInsert<-function(code, string, code.insert){
  split<-str_locate(code, fixed(string))[2]
  str_sub(code, split+1, split)<-code.insert
  return(code)
}

codeReplace<-function(code, string, code.replace){
  
  #Seems a bit pointless but will probably need to add extra functionality
  
  return(str_replace(code, fixed(string), code.replace))
  
}

#codeInsert("mary had a little lamb", "little", " blue")

#codeReplace("mary had a little lamb", "little", "blue")