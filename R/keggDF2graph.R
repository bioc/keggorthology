
keggDF2graph = function(df, root="KO.June07root") {
 dfn = names(df)
 if (!(all(c("lev1", "lev2", "term", "tag", "depth") %in% dfn))) stop(
        "data frame input must have columns lev1, lev2, term, tag, depth")
 ss = split(df, df$lev1)
 sss = lapply(ss, function(x) split(x, x$lev2))
# start graph
 KOgraph = new("graphNEL", nodes=c(root, df$term), edgemode="directed")
 lev1 = df$term[df$depth==1]
# add top level terms
 KOgraph = addEdge( root, lev1, KOgraph )
# add second and third level terms
 for (i in 1:length(sss)) {
#
   curtop = sss[[i]][[1]]$term
   for (j in 2:length(sss[[i]])) {
     tmp = sss[[i]][[j]]
     cursec = tmp[tmp$depth==2,"term"]
     KOgraph = addEdge( curtop, cursec, KOgraph )
     curth = tmp[tmp$depth==3,"term"]
     KOgraph = addEdge( cursec, curth, KOgraph )
     }
  }
 nodeDataDefaults(KOgraph, "tag") = "NONE"
 nodeData(KOgraph, df$term, "tag") <- df$tag
 nodeDataDefaults(KOgraph, "depth") = 0
 nodeData(KOgraph, df$term, "depth") <- as.numeric(df$depth)
 KOgraph
}

