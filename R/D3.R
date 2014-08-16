

#' D3 Dendrogram
#'
#' Creates a html file containing json file and a D3.js Dendrogram
#'
#' @param JSON A json object
#' @param text the text size in the dendrogram 
#' @param height the height of the dendrogram
#' @param weidth the width of the dendrogram
#' @param the location and name for the output html file
#' @author James Thomson
#' @examples hc <- hclust(dist(USArrests), "ave")
#' plot(hc)
#' JSON<-HCtoJSON(hc)
#' D3Dendro(JSON, file_out="USArrests_Dendo.html")
#' 

D3Dendro<-function(JSON, text=15, height=800, width=700, file_out){
  
  if (JSON$Type!="json:nested"){stop("Incorrect json type for this D3")}
  
  header<-paste0("<!DOCTYPE html>
                 <meta charset=\"utf-8\">
                 <style>
                 
                 .node circle {
                 fill: #fff;
                 stroke: steelblue;
                 stroke-width: 1.5px;
                 }
                 
                 .node {
                 font: ",text , "px sans-serif;
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
#' JSON<-jsonSankey(ClustComp)
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
#' @author Simon Raper and James Thomson
#' @examples 
#' nodes.df<-data.frame(name=c("Dan", "Digby", "Lex", "Flamer", "Stripey"), age=c(32, 38, 45, 17, 2))
#' links.df<-data.frame(source=c("Dan", "Digby", "Flamer"), target=c("Lex", "Flamer", "Stripey"))
#' JSON<-jsonNodesLinks(nodes.df, links.df)
#' D3Force(JSON, file_out="Force.html")
#' 

D3Force<-function(JSON, file_out){
  
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
  