

#' Multiple results to json
#'
#' Creates a json file from a sequence of events. This json will work with D3Sankey
#'
#' @param df
#' @param mode
#' @author Simon Raper 
#' @examples 

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
      # for.json[[length(for.json)+1]]<-list(df[j,])
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
    
    if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
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
  
  return(list(Type="json:compare", json=JSON))
}



#' Nodes and Linkes json
#'
#' Creates a json file with nodes and links. This json will work with D3Force
#'
#' @param nodes A datafrmae containing the nodes
#' @oaran links A dataframe containing the links
#' @author 
#' @examples 

jsonNodesLinks-function(nodes, links){
 
  
  
}

