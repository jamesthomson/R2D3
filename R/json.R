

#' Covenrts data to json form
#'
#' Creates a json file from an input
#'
#' @param df a data.frame to be converted 
#' @param mode there are three modes "vector", "coords" , "rowToObject"
#' @author Simon Raper 
#' @examples example

dfToJSON<-function(df, mode='vector'){
  
  colToList<-function(x, y){
    
    col.json<-list()
    
    #Build up a list of coordinates
    
    for (i in 1:length(x)){
      
      ni<-list(x=x[i], y=y[i])
      col.json[[length(col.json)+1]]<-ni
    }
    
    return(col.json)
    
  }
  
  
  if (mode=='vector'){
    
    for.json<-list()
    
    for (j in 1:ncol(df)){
      for.json[[length(for.json)+1]]<-list(name=colnames(df)[j] , data=df[,j])
    }
    
  }
  
  if (mode=='coords') {
    
    for.json<-list()
    
    for (j in 2:ncol(df)){
      for.json[[length(for.json)+1]]<-list(name=colnames(df)[j] , data=colToList(x=df[,1], y=df[,j]))
    }
    
  }
  
  if (mode=='rowToObject') {
    
    for.json<-list()
    
    for (j in 1:nrow(df)){
      for.json[[length(for.json)+1]]<-df[j,]
    }
    
  }
  
  jj<-toJSON(for.json)
  
  return(jj)
  
}




#' Hierachical Cluster to json
#'
#' Creates a json file from a hierachical clustering output
#'
#' @param hc A hieracichal cluster (hclust) output object
#' @author James Thomson
#' @examples hc <- hclust(dist(USArrests), "ave")
#' plot(hc)
#' JSON<-jsonHC(hc)
#' 

jsonHC<-function(hc){
  
  labels<-hc$labels
  merge<-data.frame(hc$merge)
  
  for (i in (1:nrow(merge))) {
    
    if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]], size=1),list(name=labels[-merge[i,2]], size=1)))")))}
    else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]], size=1)))")))}
    else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]], size=1), node", merge[i,2],"))")))}
    else if (merge[i,1]>0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
  }
  
  eval(parse(text=paste0("JSON<-toJSON(node",nrow(merge)-1, ")")))
  
  return(list(Type="json:nested", json=JSON))
}


#' Multiple results to json
#'
#' Creates a json file from a sequence of events. This json will work with D3Sankey
#'
#' @param data A data frame with first column the record identifier and a series of columns showig the allocations to compare
#' @author Simon Raper and James Thomson
#' @examples hc.ave <- hclust(dist(USArrests), "ave")
#' hc.single <- hclust(dist(USArrests), "single")
#' hc.ward <- hclust(dist(USArrests), "ward.D")
#' 
#' cut.ave<-cutree(hc.ave, k=6)
#' cut.single<-cutree(hc.single, k=6)
#' cut.ward<-cutree(hc.ward, k=6)
#' 
#' ClustComp<-data.frame(States=rownames(USArrests), ave=as.vector(cut.ave),single=as.vector(cut.single),ward=as.vector(cut.ward))
#' 
#' JSON<-jsonCompare(ClustComp)

jsonCompare<-function(data){
  
  data<-ClustComp
 
  nodes<-NULL
  for (i in (2:ncol(data))){
    temp<-data.frame(cluster=paste0(colnames(data)[i] ,".", data[,i]), ind=data[,1])
    nodes<-rbind(nodes, temp)
  }  
  nodes.ls<-ddply(.data=nodes, .(cluster), .fun= function(x) paste0(x[,2], collapse=" \n "))
  names(nodes.ls)<-c("name", "ind")
    
  
  edges<-NULL
  for (i in (2:(ncol(data)-1))){
    temp<-data.frame(table(paste0(colnames(data)[i],".", data[,i]), paste0(colnames(data)[i+1],".", data[,i+1])))
    edges<-rbind(edges, temp)
    
  }
  colnames(edges)<-c("source", "target", "value")
  edges<-edges[edges$value!=0,]
  
  links<-NULL
  for (i in 1:nrow(edges )){
    s<-which(as.character(nodes.ls$name)==as.character(edges$source[i]))-1
    t<-which(as.character(nodes.ls$name)==as.character(edges$target[i]))-1
    links<-rbind(links, data.frame(source=s,target=t, value=edges[i,3]))
  }
  
    
  n<-dfToJSON(nodes.ls,  'rowToObject')
  e<-dfToJSON(links,  'rowToObject')
 
  json<-paste0("{ \"nodes\":", n, ", \"links\": ", e, "}")
  
  return(list(Type="json:compare", json=json))
}



#' Nodes and Linkes json
#'
#' Creates a json file with nodes and links. This json will work with D3Force
#'
#' @param nodes A dataframe containing the nodes. One of the columns should be labelled 'name'. The rest of the columns can be any node attribute.
#' @param links A dataframe containing the links. This should consists of two columns: source and target. 
#' These should be populated with names that are in the names column of the nodes table. An optional weight column can also be included.
#' @author Simon Raper
#' @examples 
#' nodes.df<-data.frame(name=c("Dan", "Digby", "Lex", "Flamer", "Stripey"), age=c(32, 38, 45, 17, 2))
#' links.df<-data.frame(source=c("Dan", "Digby", "Flamer"), target=c("Lex", "Flamer", "Stripey"))
#' jsonNodesLinks(nodes.df, links.df)

jsonNodesLinks<-function(nodes, links){
  
  nodes<-data.frame(lapply(nodes, as.character), stringsAsFactors=FALSE)
  links<-data.frame(lapply(links, as.character), stringsAsFactors=FALSE)
  links.lu<-NULL
  
  for (i in 1:nrow(links)){
    
    s<-which(nodes$name==links$source[i])-1
    t<-which(nodes$name==links$target[i])-1
    if (ncol(links)>2) 
    {w<-links$weight[i]}
    else
    {w<-1}
    
    links.lu<-rbind(links.lu, c(s, t, w))
    
  }
  
  names(links.lu)<-c("source", "target", "value")
  
  n<-dfToJSON(nodes,  'rowToObject')
  e<-dfToJSON(links.lu,  'rowToObject')
 
  json<-paste0("graph={ \"nodes\":", n, ", \"links\": ", e, "}")
  return(list(Type="json:nodes_links", json=json))
  
}





#' Data frame to nested json
#'
#' Creates a nested json file from a data frame
#'
#' @param data A data frame to be converted to a nested json. Columns need to be in order of nesting; top level on the left, bottom level on the right. Columns must contain text.
#' @param top_label The label assigned to the top leve or first node. By default its "Top"
#' @author James Thomson
#' @examples data<-data.frame(
#'Level1=c('Parent A','Parent A','Parent A','Parent A','Parent A','Parent A','Parent A','Parent A','Parent A','Parent A','Parent A','Parent A','Parent B','Parent B','Parent B','Parent B','Parent B','Parent B','Parent B','Parent C','Parent C','Parent C','Parent C','Parent C','Parent C'),
#'Level2=c('Child A','Child A','Child A','Child A','Child B','Child B','Child C','Child C','Child C','Child C','Child C','Child C','Child D','Child D','Child D','Child D','Child E','Child E','Child E','Child F','Child G','Child G','Child G','Child G','Child G'),  
#'Level3=c('Sub Child 1','Sub Child 2','Sub Child 3','Sub Child 4','Sub Child 5','Sub Child 6','Sub Child 7','Sub Child 8','Sub Child 9','Sub Child 10','Sub Child 11','Sub Child 12','Sub Child 13','Sub Child 14','Sub Child 15','Sub Child 16','Sub Child 17','Sub Child 18','Sub Child 19','Sub Child 20','Sub Child 21','Sub Child 22','Sub Child 23','Sub Child 24','Sub Child 25')
#')

#' JSON<-jsonNestedData(data=data, top_label="Top Level Label")
#' 
#' 

jsonNestedData<-function(data, top_label="Top") {
  
  #bottom level   
  labels<-data.frame(table(data[,ncol(data)-1]))
  for (i in c(1:nrow(labels))) {
    items<-data[data[,ncol(data)-1]==labels[i,1],ncol(data)]
    eval(parse(text=paste0(gsub(" ", "_",labels[i,1]),"<-list(name=\"", labels[i,1], "\", children=list(", paste0("list(name=as.character(items[", c(1:length(items)), "]))", collapse=","),  "))")))
  }
  
  #iterate through other levels
  for (c in c((ncol(data)-2):1)) {
    labels<-data.frame(table(data[,c]))        
    lookup<-data.frame(table(data[,c], data[,c+1]))
    lookup2<-lookup[lookup$Freq!=0,]
    for (i in c(1:nrow(labels))) {
      eval(parse(text=paste0(gsub(" ", "_",labels[i,1]),
                             "<-list(name=\"", 
                             labels[i,1], 
                             paste0("\", children=list(", 
                                    paste(gsub(" ", "_", lookup2[lookup2$Var1==labels[i,1],2]), collapse=","), ")"),
                             ")")
      ))
    }
  }
  
  #final top level
  labels<-data.frame(table(data[,1]))
  eval(parse(text=paste0("Top<-list(name=\"", top_label,"\" , children=list(", paste(gsub(" ", "_", labels[,1]), collapse=","), ")",")")))           
  json<-toJSON(Top)

  
  return(list(Type="json:nested", json=json))
}
