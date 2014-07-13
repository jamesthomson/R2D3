#' Hierachical Cluster to json
#'
#' Creates a json file from a hierachical clustering output
#'
#' @param hc A hieracichal cluster (hclust) output object
#' @author James Thomson
#' @examples hc <- hclust(dist(USArrests), "ave")
#' plot(hc)
#' JSON<-HCtoJSON(hc)
#' 

HCtoJSON<-function(hc){
  
  labels<-hc$labels
  merge<-data.frame(hc$merge)
  
  for (i in (1:nrow(merge))) {
    
    if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
    else if (merge[i,1]>0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
  }
  
  eval(parse(text=paste0("JSON<-toJSON(node",nrow(merge)-1, ")")))
  
  return(JSON)
}


#' Nodes and Edges to json
#'
#' Creates a json file from a sequence of events
#'
#' @param data A data frame with first column the record identifier and a series of columns showig the allocations to compare
#' @author James Thomson
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
#' JSON<-NandEjson(ClustComp)

NandEjson<-function(data){
  
  data<-ClustComp
  
  edges<-NULL
  for (i in (2:(ncol(data)-1))){
    temp<-data.frame(table(paste0(colnames(data)[i],".", data[,i]), paste0(colnames(data)[i+1],".", data[,i+1])))
    edges<-rbind(edges, temp)
    
  }
  colnames(edges)<-c("from", "to", "freq")
  edges<-edges[edges$freq!=0,]
  
  nodes<-NULL
  for (i in (2:ncol(data))){
    temp<-data.frame(table(paste0(colnames(data)[i],".", data[,i])))
    nodes<-rbind(nodes, temp)
  }
  
  nested_nodes<-NULL
  for (i in (2:ncol(data))){
    temp<-data.frame(table(paste0(colnames(data)[i],".", data[,i])))
    nodes<-rbind(nodes, temp)
  }  
  
  
  
  colnames(nodes)<-c("nodes", "freq")
  
  json<-paste0(toJSON(nodes), toJSON(edges))
  return (json)
}



