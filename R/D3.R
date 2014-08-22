

#' D3 Dendrogram
#'
#' Creates a html file containing json file and a D3.js Dendrogram
#'
#' @param JSON A json object
#' @param height the height of the dendrogram
#' @param width the width of the dendrogram
#' @param radial.diameter The diameter of your radial visualisation. Only needed (and used) when radial=TRUE
#' @param collapsible Logical TRUE/FALSE as to whether you want the visualisation to be collapsible with a single-click on the nodes
#' @param radial Logical TRUE/FALSE as to whether you want the dendrogram to be radial.
#' @param file_out the location and name for the output html file
#' @author James Thomson & Andrew Patterson
#' @references Mike Bostock's lovely d3: http://d3js.org/ and Captain Anonymous' collapsible radial dendrogram here: http://codepen.io/anon/pen/xItvw
#' @examples hc <- hclust(dist(USArrests), "ave")
#' plot(hc)
#' JSON<-jsonHC(hc)
#' D3Dendro(JSON,width=400, file_out="USArrests_Dendo.html")
#' D3Dendro(JSON,collapsible=TRUE,file_out="USArrests_Dendo_collapse.html")
#' D3Dendro(JSON,collapsible=FALSE,radial=TRUE, file_out="USArrests_Dendo_radial.html")
#' D3Dendro(JSON,collapsible=TRUE,radial=TRUE,file_out="USArrests_Dendo_collapse_radial.html")
#' 

D3Dendro<-function(JSON, file_out, height=800, width=700, radial.diameter = 1600, collapsible=FALSE, radial=FALSE){
  
  if (JSON$Type!="json:nested"){stop("Incorrect json type for this D3")}
  if (!(is.logical(collapsible)) | !(is.logical(radial))) {stop("Incorrect collapsible and/or radial argument. Should be TRUE or FALSE")}
  
  header<-paste0("<!DOCTYPE html>
                 <meta charset=\"utf-8\">
                 <style>
                 
                 .node circle {
                 fill: #fff;
                 stroke: steelblue;
                 stroke-width: 1.5px;
                 }
                 
                 .node {
                 font: 15px sans-serif;
                 }
                 
                 .link {
                 fill: none;
                 stroke: #ccc;
                 stroke-width: 1.5px;
                 }
                 
                 </style>
                 <body>
                 <script src=\"http://d3js.org/d3.v3.min.js\"></script>
                 
                 <script type=\"application/json\" id=\"data\">")
  
  if (collapsible==FALSE & radial==FALSE) {
    footer<-paste0("</script>
                   
                   
                   
                   
                   <script>
                   
                   var data = document.getElementById('data').innerHTML;
                   root = JSON.parse(data);
                   
                   
                   var width = ", width, ",
                   height = ", height, ";
                   
                   var cluster = d3.layout.cluster()
                   .size([height-50, width - 160]);
                   
                   var diagonal = d3.svg.diagonal()
                   .projection(function(d) { return [d.y, d.x]; });
                   
                   var svg = d3.select(\"body\").append(\"svg\")
                   .attr(\"width\", width)
                   .attr(\"height\", height)
                   .append(\"g\")
                   .attr(\"transform\", \"translate(40,0)\");
                   
                   
                   var nodes = cluster.nodes(root),
                   links = cluster.links(nodes);
                   
                   var link = svg.selectAll(\".link\")
                   .data(links)
                   .enter().append(\"path\")
                   .attr(\"class\", \"link\")
                   .attr(\"d\", diagonal);
                   
                   var node = svg.selectAll(\".node\")
                   .data(nodes)
                   .enter().append(\"g\")
                   .attr(\"class\", \"node\")
                   .attr(\"transform\", function(d) { return \"translate(\" + d.y + \",\" + d.x + \")\"; })
                   
                   node.append(\"circle\")
                   .attr(\"r\", 4.5);
                   
                   node.append(\"text\")
                   .attr(\"dx\", function(d) { return d.children ? 8 : 8; })
                   .attr(\"dy\", function(d) { return d.children ? 20 : 4; })
                   .style(\"text-anchor\", function(d) { return d.children ? \"end\" : \"start\"; })
                   .text(function(d) { return d.name; });
                   
                   
                   d3.select(self.frameElement).style(\"height\", height + \"px\");
                   
                   </script>") 
  } 
  
  if (collapsible==TRUE & radial==FALSE) {
    footer<-paste0("</script>
                   
                   
                   
                   
                   <script>
                   
                   var margin = {top: 10, right: 200, bottom: 10, left: 100},
                   width = ", width, " - margin.right - margin.left,
                   height = ", height, " - margin.top - margin.bottom;
                   
                   var i = 0,
                   duration = 750,
                   root;
                   
                   var tree = d3.layout.tree()
                   .size([height, width]);
                   
                   var diagonal = d3.svg.diagonal()
                   .projection(function(d) { return [d.y, d.x]; });
                   
                   var svg = d3.select(\"body\").append(\"svg\")
                   .attr(\"width\", width + margin.right + margin.left)
                   .attr(\"height\", height + margin.top + margin.bottom)
                   .append(\"g\")
                   .attr(\"transform\", \"translate(\" + margin.left + \",\" + margin.top + \")\");
                   
                   var data = document.getElementById('data').innerHTML;
                   root = JSON.parse(data);
                   root.x0 = height / 2;
                   root.y0 = 0;
                   
                   function collapse(d) {
                   if (d.children) {
                   d._children = d.children;
                   d._children.forEach(collapse);
                   d.children = null;
                   }
                   }
                   
                   root.children.forEach(collapse);
                   update(root);
                   
                   d3.select(self.frameElement).style(\"height\", height + \"px\");
                   
                   function update(source) {
                   
                   // Compute the new tree layout.
                   var nodes = tree.nodes(root).reverse(),
                   links = tree.links(nodes);
                   
                   // Normalize for fixed-depth.
                   // nodes.forEach(function(d) { d.y = d.depth * 180; });
                   
                   // Update the nodes…
                   var node = svg.selectAll(\"g.node\")
                   .data(nodes, function(d) { return d.id || (d.id = ++i); });
                   
                   // Enter any new nodes at the parent's previous position.
                   var nodeEnter = node.enter().append(\"g\")
                   .attr(\"class\", \"node\")
                   .attr(\"transform\", function(d) { return \"translate(\" + source.y0 + \",\" + source.x0 + \")\"; })
                   .on(\"click\", click);
                   
                   nodeEnter.append(\"circle\")
                   .attr(\"r\", 1e-6)
                   .style(\"fill\", function(d) { return d._children ? \"lightsteelblue\" : \"#fff\"; });
                   
                   nodeEnter.append(\"text\")
                   .attr(\"x\", function(d) { return d.children || d._children ? -10 : 10; })
                   .attr(\"dy\", \".35em\")
                   .attr(\"text-anchor\", function(d) { return d.children || d._children ? \"end\" : \"start\"; })
                   .text(function(d) { return d.name; })
                   .style(\"fill-opacity\", 1e-6);
                   
                   // Transition nodes to their new position.
                   var nodeUpdate = node.transition()
                   .duration(duration)
                   .attr(\"transform\", function(d) { return \"translate(\" + d.y + \",\" + d.x + \")\"; });
                   
                   nodeUpdate.select(\"circle\")
                   .attr(\"r\", 4.5)
                   .style(\"fill\", function(d) { return d._children ? \"lightsteelblue\" : \"#fff\"; });
                   
                   nodeUpdate.select(\"text\")
                   .style(\"fill-opacity\", 1);
                   
                   // Transition exiting nodes to the parent's new position.
                   var nodeExit = node.exit().transition()
                   .duration(duration)
                   .attr(\"transform\", function(d) { return \"translate(\" + source.y + \",\" + source.x + \")\"; })
                   .remove();
                   
                   nodeExit.select(\"circle\")
                   .attr(\"r\", 1e-6);
                   
                   nodeExit.select(\"text\")
                   .style(\"fill-opacity\", 1e-6);
                   
                   // Update the links…
                   var link = svg.selectAll(\"path.link\")
                   .data(links, function(d) { return d.target.id; });
                   
                   // Enter any new links at the parent's previous position.
                   link.enter().insert(\"path\", \"g\")
                   .attr(\"class\", \"link\")
                   .attr(\"d\", function(d) {
                   var o = {x: source.x0, y: source.y0};
                   return diagonal({source: o, target: o});
                   });
                   
                   // Transition links to their new position.
                   link.transition()
                   .duration(duration)
                   .attr(\"d\", diagonal);
                   
                   // Transition exiting nodes to the parent's new position.
                   link.exit().transition()
                   .duration(duration)
                   .attr(\"d\", function(d) {
                   var o = {x: source.x, y: source.y};
                   return diagonal({source: o, target: o});
  })
                   .remove();
                   
                   // Stash the old positions for transition.
                   nodes.forEach(function(d) {
                   d.x0 = d.x;
                   d.y0 = d.y;
                   });
  }
                   
                   // Toggle children on click.
                   function click(d) {
                   if (d.children) {
                   d._children = d.children;
                   d.children = null;
                   } else {
                   d.children = d._children;
                   d._children = null;
                   }
                   update(d);
                   }
                   
                   </script>")
    
}

if (collapsible==FALSE & radial==TRUE) {
  footer<-paste0("</script>
                 
                 
                 
                 
                 <script>
                 var radius = ",radial.diameter," / 2;
                 
                 var cluster = d3.layout.cluster()
                 .size([360, radius - 120]);
                 
                 var diagonal = d3.svg.diagonal.radial()
                 .projection(function(d) { return [d.y, d.x / 180 * Math.PI]; });
                 
                 var svg = d3.select(\"body\").append(\"svg\")
                 .attr(\"width\", radius * 2 + 100)
                 .attr(\"height\", radius * 2 + 100)
                 .append(\"g\")
                 .attr(\"transform\", \"translate(\" + radius + \",\" + radius + \")\");
                 
                 var data = document.getElementById('data').innerHTML;
                 root = JSON.parse(data);
                 var nodes = cluster.nodes(root);
                 
                 var link = svg.selectAll(\"path.link\")
                 .data(cluster.links(nodes))
                 .enter().append(\"path\")
                 .attr(\"class\", \"link\")
                 .attr(\"d\", diagonal);
                 
                 var node = svg.selectAll(\"g.node\")
                 .data(nodes)
                 .enter().append(\"g\")
                 .attr(\"class\", \"node\")
                 .attr(\"transform\", function(d) { return \"rotate(\" + (d.x - 90) + \")translate(\" + d.y + \")\"; })
                 
                 node.append(\"circle\")
                 .attr(\"r\", 4.5);
                 
                 node.append(\"text\")
                 .attr(\"dy\", \".31em\")
                 .attr(\"text-anchor\", function(d) { return d.x < 180 ? \"start\" : \"end\"; })
                 .attr(\"transform\", function(d) { return d.x < 180 ? \"translate(8)\" : \"rotate(180)translate(-8)\"; })
                 .text(function(d) { return d.name; });
                 
                 d3.select(self.frameElement).style(\"height\", radius * 2 + \"px\");
                 
                 
                 </script>")
  
}

if (collapsible==TRUE & radial==TRUE) {
  footer<-paste0("</script>
                 
                 
                 
                 
                 <script>
                 var diameter = ",radial.diameter,";
                 
                 var margin = {top: 10, right: 200, bottom: 10, left: 100},
                 width = diameter+500,
                 height = diameter+500;
                 
                 var i = 0,
                 duration = 350,
                 root;
                 
                 var tree = d3.layout.tree()
                 .size([360, diameter / 2 - 80])
                 .separation(function(a, b) { return (a.parent == b.parent ? 1 : 10) / a.depth; });
                 
                 var diagonal = d3.svg.diagonal.radial()
                 .projection(function(d) { return [d.y, d.x / 180 * Math.PI]; });
                 
                 var svg = d3.select(\"body\").append(\"svg\")
                 .attr(\"width\", width )
                 .attr(\"height\", height )
                 .append(\"g\")
                 .attr(\"transform\", \"translate(\" + diameter / 2 + \",\" + diameter / 2 + \")\");
                 
                 var data = document.getElementById('data').innerHTML;
                 root = JSON.parse(data);
                 root.x0 = height / 2;
                 root.y0 = 0;
                 
                 //root.children.forEach(collapse); // start with all children collapsed
                 update(root);
                 
                 d3.select(self.frameElement).style(\"height\", \"800px\");
                 
                 function update(source) {
                 
                 // Compute the new tree layout.
                 var nodes = tree.nodes(root),
                 links = tree.links(nodes);
                 
                 // Normalize for fixed-depth.
                 // nodes.forEach(function(d) { d.y = d.depth * 80; });
                 
                 // Update the nodes…
                 var node = svg.selectAll(\"g.node\")
                 .data(nodes, function(d) { return d.id || (d.id = ++i); });
                 
                 // Enter any new nodes at the parent's previous position.
                 var nodeEnter = node.enter().append(\"g\")
                 .attr(\"class\", \"node\")
                 //.attr(\"transform\", function(d) { return \"rotate(\" + (d.x - 90) + \")translate(\" + d.y + \")\"; })
                 .on(\"click\", click);
                 
                 nodeEnter.append(\"circle\")
                 .attr(\"r\", 1e-6)
                 .style(\"fill\", function(d) { return d._children ? \"lightsteelblue\" : \"#fff\"; });
                 
                 nodeEnter.append(\"text\")
                 .attr(\"x\", 10)
                 .attr(\"dy\", \".35em\")
                 .attr(\"text-anchor\", \"start\")
                 //.attr(\"transform\", function(d) { return d.x < 180 ? \"translate(0)\" : \"rotate(180)translate(-\" + (d.name.length * 8.5)  + \")\"; })
                 .text(function(d) { return d.name; })
                 .style(\"fill-opacity\", 1e-6);
                 
                 // Transition nodes to their new position.
                 var nodeUpdate = node.transition()
                 .duration(duration)
                 .attr(\"transform\", function(d) { return \"rotate(\" + (d.x - 90) + \")translate(\" + d.y + \")\"; })
                 
                 nodeUpdate.select(\"circle\")
                 .attr(\"r\", 4.5)
                 .style(\"fill\", function(d) { return d._children ? \"lightsteelblue\" : \"#fff\"; });
                 
                 nodeUpdate.select(\"text\")
                 .style(\"fill-opacity\", 1)
                 .attr(\"transform\", function(d) { return d.x < 180 ? \"translate(0)\" : \"rotate(180)translate(-\" + (d.name.length + 50)  + \")\"; });
                 
                 // TODO: appropriate transform
                 var nodeExit = node.exit().transition()
                 .duration(duration)
                 //.attr(\"transform\", function(d) { return \"diagonal(\" + source.y + \",\" + source.x + \")\"; })
                 .remove();
                 
                 nodeExit.select(\"circle\")
                 .attr(\"r\", 1e-6);
                 
                 nodeExit.select(\"text\")
                 .style(\"fill-opacity\", 1e-6);
                 
                 // Update the links…
                 var link = svg.selectAll(\"path.link\")
                 .data(links, function(d) { return d.target.id; });
                 
                 // Enter any new links at the parent's previous position.
                 link.enter().insert(\"path\", \"g\")
                 .attr(\"class\", \"link\")
                 .attr(\"d\", function(d) {
                 var o = {x: source.x0, y: source.y0};
                 return diagonal({source: o, target: o});
                 });
                 
                 // Transition links to their new position.
                 link.transition()
                 .duration(duration)
                 .attr(\"d\", diagonal);
                 
                 // Transition exiting nodes to the parent's new position.
                 link.exit().transition()
                 .duration(duration)
                 .attr(\"d\", function(d) {
                 var o = {x: source.x, y: source.y};
                 return diagonal({source: o, target: o});
})
                 .remove();
                 
                 // Stash the old positions for transition.
                 nodes.forEach(function(d) {
                 d.x0 = d.x;
                 d.y0 = d.y;
                 });
                 }
                 
                 // Toggle children on click.
                 function click(d) {
                 if (d.children) {
                 d._children = d.children;
                 d.children = null;
                 } else {
                 d.children = d._children;
                 d._children = null;
                 }
                 
                 update(d);
                 }
                 
                 // Collapse nodes
                 function collapse(d) {
                 if (d.children) {
                 d._children = d.children;
                 d._children.forEach(collapse);
                 d.children = null;
                 }
                 }
                 
                 
                 </script>")
    
  }
  
  fileConn<-file(file_out)
  writeLines(paste0(header, JSON$json, footer), fileConn)
  close(fileConn)
  
}


#' D3 Sankey
#'
#' Creates a html file containing json file and a D3.js Sankey
#'
#' @param JSON A json object
#' @param the location and name for the output html file
#' @author Simon Raper and James Thomson
#' @references Mike Bostock's lovely d3: http://d3js.org/
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
#' D3Sankey(JSON, file_out="Sankey.html")
#' 

D3Sankey<-function(JSON, file_out){

  if (JSON$Type!="json:compare"){stop("Incorrect json type for this D3")}

  header<-"<html>\n<head>\n<title> Sankey Chart </title>\n<script src=\"http://d3js.org/d3.v3.min.js\"></script>
                \n<script src=\"http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js\"></script>
                                \n<script src=\"http://bost.ocks.org/mike/sankey/sankey.js\"></script>
    \n<script type=\"application/json\" id=\"crime\">\n\n\n"
  
  footer<-"\n\n\n
</script>
 
  <style>
  
  #chart {
  height: 2000px;
  
}
  .node rect {
  cursor: move;
  fill-opacity: .9;
  shape-rendering: crispEdges;
  }
  .node text {
  pointer-events: none;
  }
  .link {
  fill: none;
  stroke: #000;
  stroke-opacity: .2;
  }
  .link:hover {
  stroke-opacity: .5;
  }
  .d3-tip {
  line-height: 1;
  color: #000;
  font-size: 10px;
  }
  .cursor {
  fill: none;
  stroke: brown;
  pointer-events: none;
  }
  
  </style>
  
  </head>
  
  <body>
  
  <div id=\"chart\"> </div>
  
  <script>
  
  var crime = document.getElementById('crime').innerHTML;
  energy = JSON.parse(crime);
  
  var margin = {
  top: 200,
  right: 1,
  bottom: 6,
  left: 100
  },
  width = 700 - margin.left - margin.right,
  height = 2000 - margin.top - margin.bottom;
  
  var formatNumber = d3.format(\",.0f\"),
  format = function (d) {
  return formatNumber(d) + \" Items\";
  },
  color = d3.scale.category20();
  
  
  
  
  
  
  var svg = d3.select(\"#chart\").append(\"svg\")
  .attr(\"width\", width + margin.left + margin.right)
  .attr(\"height\", height + margin.top + margin.bottom)
  .append(\"g\")
  .attr(\"transform\", \"translate(\" + margin.left + \",\" + margin.top + \")\");
  
  
  
  var sankey = d3.sankey()
  .nodeWidth(15)
  .nodePadding(10)
  .size([width, height]);
  
  var path = sankey.link();
  
  sankey.nodes(energy.nodes)
  .links(energy.links)
  .layout(32);
  
  
  
  var link = svg.append(\"g\").selectAll(\".link\")
  .data(energy.links)
  .enter().append(\"path\")
  .attr(\"class\", \"link\")
  .attr(\"d\", path)
  .style(\"stroke-width\", function (d) {
  return Math.max(1, d.dy);
  })
  .sort(function (a, b) {
  return b.dy - a.dy;
  });
  
  link.append(\"title\")
  .text(function (d) {
  return \"From Cluster \" + d.source.name + \" to Cluster \" + d.target.name + \"\\n\" + format(d.value) ;
  });
  
  var node = svg.append(\"g\").selectAll(\".node\")
  .data(energy.nodes)
  .enter().append(\"g\")
  .attr(\"class\", \"node\")
  .attr(\"transform\", function (d) {
  return \"translate(\" + d.x + \",\" + d.y + \")\";
  })
  .call(d3.behavior.drag()
  .origin(function (d) {
  return d;
  })
  .on(\"dragstart\", function () {
  this.parentNode.appendChild(this);
  })
  .on(\"drag\", dragmove));
  
  node.append(\"rect\")
  .attr(\"height\", function (d) {
  return d.dy;
  })
  .attr(\"width\", sankey.nodeWidth())
  .style(\"fill\", \"#4679BD\")
  
  .append(\"title\")
  .text(function (d) {
  return d.name + \" - \" + format(d.value) + \"\\n\" +  d.ind ;
  });
  
  node.append(\"text\")
    .attr(\"x\", -6)
    .attr(\"y\", function (d) {
    return d.dy / 2;
})
    .attr(\"dy\", \".35em\")
    .attr(\"text-anchor\", \"end\")
    .attr(\"transform\", null)
    .text(function (d) {
    return d.name;
})
    .filter(function (d) {
    return d.x < width / 2;
})
    .attr(\"x\", 6 + sankey.nodeWidth())
    .attr(\"text-anchor\", \"start\");
 
function dragmove(d) {
    d3.select(this).attr(\"transform\", \"translate(\" + d.x + \",\" + (d.y = Math.max(0, Math.min(height - d.dy, d3.event.y))) + \")\");
    sankey.relayout();
    link.attr(\"d\", path);
}
 
 
</script>
   
</body>
 
"
 
  
  fileConn<-file(file_out)
  writeLines(paste0(header, JSON$json, footer), fileConn)
  close(fileConn)
  
}  



#' D3 Force
#'
#' Creates a html file containing json file and a D3.js Force Directed Layout.
#' If you want toe colours in teh force directed layout to represent a group. PLease ensure the column is labelled "group" in the data frame
#'
#' @param JSON A json object
#' @param the location and name for the output html file
#' @param options A list of features to include the graph (see the details section)
#' @author Simon Raper and James Thomson
#' @references Mike Bostock's lovely d3: http://d3js.org/
#' @examples 
#' nodes.df<-data.frame(name=c("Dan", "Digby", "Lex", "Flamer", "Stripey"), age=c(32, 38, 45, 17, 2))
#' links.df<-data.frame(source=c("Dan", "Digby", "Flamer"), target=c("Lex", "Flamer", "Stripey"))
#' JSON<-jsonNodesLinks(nodes.df, links.df)
#' D3Force(JSON, file_out="Force.html")
#' 
#' #With directional arrows
#' D3Force(JSON, file_out="Force.html", options=list(arrows=TRUE))
#' 
#' data(celebs)
#' colnames(celebs$relationships)<-c('source', 'target')
#' colnames(celebs$celebs)<-c('name', 'group')
#' JSON<-jsonNodesLinks(celebs$celebs, celebs$relationships)
#' D3Force(JSON, file_out="/Users/home/Documents/R_Projects/Force.html")
#' 

D3Force<-function(JSON, file_out, options){
  
  if (JSON$Type!="json:nodes_links"){stop("Incorrect json type for this D3")}

header<-"<!DOCTYPE html>

<style>

.link {
  stroke: #ccc;
}

.node text {
  pointer-events: none;
  font: 10px sans-serif;
}


</style>
<body>
<script src=\"http://d3js.org/d3.v3.min.js\"></script>

<script type='text/javascript' src=\"http://labratrevenge.com/d3-tip/javascripts/d3.tip.v0.6.3.js\"> </script>

<script type=\"application/json\" id=\"mis\">"

footer<-"</script>




<script>
//Constants for the SVG
var width = 800,
height = 800;

//Set up the colour scale
var color = d3.scale.category20();

//Set up the force layout
var force = d3.layout.force()
.charge(-120)
.linkDistance(80)
.size([width, height]);

//Append a SVG to the body of the html page. Assign this SVG as an object to svg
var svg = d3.select(\"body\").append(\"svg\")
.attr(\"width\", width)
.attr(\"height\", height);

//Read the data from the mis element 
var mis = document.getElementById('mis').innerHTML;
graph = JSON.parse(mis);

//Creates the graph data structure out of the json data
force.nodes(graph.nodes)
.links(graph.links)
.start();

//Create all the line svgs but without locations yet
var link = svg.selectAll(\".link\")
.data(graph.links)
.enter().append(\"line\")
.attr(\"class\", \"link\")
.style(\"stroke-width\", function (d) {
return Math.sqrt(d.value);
});

//Do the same with the circles for the nodes - no 
//Changed
var node = svg.selectAll(\".node\")
.data(graph.nodes)
.enter().append(\"g\")
.attr(\"class\", \"node\")
.call(force.drag);

node.append(\"circle\")
.attr(\"r\", 8)
.style(\"fill\", function (d) {
return color(d.group);
})

node.append(\"text\")
.attr(\"dx\", 10)
.attr(\"dy\", \".35em\")
.text(function(d) { return d.name });
//End changed


//Now we are giving the SVGs co-ordinates - the force layout is generating the co-ordinates which this code is using to update the attributes of the SVG elements
force.on(\"tick\", function () {
link.attr(\"x1\", function (d) {
return d.source.x;
})
.attr(\"y1\", function (d) {
return d.source.y;
})
.attr(\"x2\", function (d) {
        return d.target.x;
    })
        .attr(\"y2\", function (d) {
        return d.target.y;
    });

    //Changed
    
    d3.selectAll(\"circle\").attr(\"cx\", function (d) {
        return d.x;
    })
        .attr(\"cy\", function (d) {
        return d.y;
    });

    d3.selectAll(\"text\").attr(\"x\", function (d) {
        return d.x;
    })
        .attr(\"y\", function (d) {
        return d.y;
    });
    
    //End Changed

});
 </script>
 </body>
 </html>"


if (options$arrows==TRUE){
  
  
  footer<-codeInsert(footer, ".attr(\"class\", \"link\")",".style(\"marker-end\",  \"url(#suit)\")")
    
  footer.add<-"
  svg.append(\"defs\").selectAll(\"marker\")
    .data([\"suit\", \"licensing\", \"resolved\"])
  .enter().append(\"marker\")
    .attr(\"id\", function(d) { return d; })
    .attr(\"viewBox\", \"0 -5 10 10\")
    .attr(\"refX\", 25)
    .attr(\"refY\", 0)
    .attr(\"markerWidth\", 6)
    .attr(\"markerHeight\", 6)
    .attr(\"orient\", \"auto\")
  .append(\"path\")
    .attr(\"d\", \"M0,-5L10,0L0,5 L10,0 L0, -5\")
    .style(\"stroke\", \"#4679BD\")
  .style(\"opacity\", \"0.6\");"
  
  footer<-codeInsert(footer, "End Changed\n\n});\n", footer.add)
}

fileConn<-file(file_out)
writeLines(paste0(header, JSON$json, footer), fileConn)
close(fileConn)

}    
  



#' D3 Tree
#'
#' Creates a html file containing json file and a D3.js Tree Map.
#' The nested json needs values assigned to it in order for it to work
#'
#' @param JSON A json object
#' @param the location and name for the output html file
#' @author James Thomson
#' @references Mike Bostock's lovely d3: http://d3js.org/
#' @examples
#' data(counties) 
#' JSON<-jsonNestedData(structure=counties[,1:3], values=counties[,4], top_label="UK")
#' D3Tree(JSON, file_out="Tree.html")
#' 

D3Tree<-function(JSON, file_out){
  
  if (JSON$Type!="json:nested"){stop("Incorrect json type for this D3")}
  
  header<-"<!DOCTYPE html>
<html>
  <head>
  <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\"/>
  <link type=\"text/css\" rel=\"stylesheet\" href=\"http://mbostock.github.io/d3/talk/20111018/style.css\"/>
  <script type=\"text/javascript\" src=\"http://mbostock.github.io/d3/talk/20111018/d3/d3.js\"></script>
  <script type=\"text/javascript\" src=\"http://mbostock.github.io/d3/talk/20111018/d3/d3.layout.js\"></script>
  <style type=\"text/css\">
  
  .chart {
  display: block;
  margin: auto;
  margin-top: 40px;
  }
  
  text {
  font-size: 11px;
  }
  
  rect {
  fill: none;
  }
  
  </style>
  </head>
  <body>
  <div id=\"body\">
  <div id=\"footer\">
        d3.layout.treemap
        <div class=\"hint\">click or option-click to descend or ascend</div>
        <div><select>
          <option value=\"size\">Size</option>
          <option value=\"count\">Count</option>
        </select></div>
      </div>
    </div>
  
	 <script type=\"application/json\" id=\"data\">"
  
  footer<-"  </script> 
	
	
  
  
  <script type=\"text/javascript\">
  
  var data = document.getElementById('data').innerHTML;
  data = JSON.parse(data);
  
  var w = 1280 - 80,
  h = 800 - 180,
  x = d3.scale.linear().range([0, w]),
  y = d3.scale.linear().range([0, h]),
  color = d3.scale.category20c(),
  root,
  node;
  
  var treemap = d3.layout.treemap()
  .round(false)
  .size([w, h])
  .sticky(true)
  .value(function(d) { return d.size; });
  
  var svg = d3.select(\"#body\").append(\"div\")
  .attr(\"class\", \"chart\")
  .style(\"width\", w + \"px\")
  .style(\"height\", h + \"px\")
  .append(\"svg:svg\")
  .attr(\"width\", w)
  .attr(\"height\", h)
  .append(\"svg:g\")
  .attr(\"transform\", \"translate(.5,.5)\");
  
  
  node = root = data;
  
  var nodes = treemap.nodes(root)
  .filter(function(d) { return !d.children; });
  
  var cell = svg.selectAll(\"g\")
  .data(nodes)
  .enter().append(\"svg:g\")
  .attr(\"class\", \"cell\")
  .attr(\"transform\", function(d) { return \"translate(\" + d.x + \",\" + d.y + \")\"; })
  .on(\"click\", function(d) { return zoom(node == d.parent ? root : d.parent); });
  
  cell.append(\"svg:rect\")
  .attr(\"width\", function(d) { return d.dx - 1; })
  .attr(\"height\", function(d) { return d.dy - 1; })
  .style(\"fill\", function(d) { return color(d.parent.name); });
  
  cell.append(\"svg:text\")
  .attr(\"x\", function(d) { return d.dx / 2; })
  .attr(\"y\", function(d) { return d.dy / 2; })
  .attr(\"dy\", \".35em\")
  .attr(\"text-anchor\", \"middle\")
  .text(function(d) { return d.name; })
  .style(\"opacity\", function(d) { d.w = this.getComputedTextLength(); return d.dx > d.w ? 1 : 0; });
  
  d3.select(window).on(\"click\", function() { zoom(root); });
  
  d3.select(\"select\").on(\"change\", function() {
  treemap.value(this.value == \"size\" ? size : count).nodes(root);
  zoom(node);
});
  
  
  function size(d) {
  return d.size;
  }
  
  function count(d) {
  return 1;
  }
  
  function zoom(d) {
  var kx = w / d.dx, ky = h / d.dy;
  x.domain([d.x, d.x + d.dx]);
  y.domain([d.y, d.y + d.dy]);
  
  var t = svg.selectAll(\"g.cell\").transition()
      .duration(d3.event.altKey ? 7500 : 750)
      .attr(\"transform\", function(d) { return \"translate(\" + x(d.x) + \",\" + y(d.y) + \")\"; });

  t.select(\"rect\")
      .attr(\"width\", function(d) { return kx * d.dx - 1; })
      .attr(\"height\", function(d) { return ky * d.dy - 1; })

  t.select(\"text\")
      .attr(\"x\", function(d) { return kx * d.dx / 2; })
      .attr(\"y\", function(d) { return ky * d.dy / 2; })
      .style(\"opacity\", function(d) { return kx * d.dx > d.w ? 1 : 0; });

  node = d;
  d3.event.stopPropagation();
}

    </script>
  </body>
</html>"
  
  fileConn<-file(file_out)
  writeLines(paste0(header, JSON$json, footer), fileConn)
  close(fileConn)
  
  }    
  