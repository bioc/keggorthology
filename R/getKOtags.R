
getKOtags = function(str, ignore.case=TRUE) {
 if (length(str) != 1) stop("single string only please; use lapply for vector")
 data(KOgraph)
 nod = grep(str, nodes(KOgraph), ignore.case=ignore.case, value=TRUE)
 unlist(nodeData(KOgraph, nod, "tag"))
}

getKOprobes = function(str, useAcc=TRUE, plat="hgu95av2", na.action=na.omit) {
#
# this is a messy function, predicated on misunderstanding of acc
#
 tags = as.character(sapply(str, getKOtags))
#
# now that i have tags, make the graph with tags as nodes
 data(KOgraph)
 tagg = KOgraph
 nodes(tagg) = unlist(nodeData(KOgraph,nodes(KOgraph), "tag"))
# make all nodes point to themselves
 tagg = addEdge( nodes(tagg), nodes(tagg), tagg)
 if (useAcc) {
     lkacc = acc(tagg, tags) # fails for selfloop
     names(lkacc) = NULL
     tmp = unlist(lkacc)
     }
 if (length(tmp) > 0) tags = union(names(tmp), tags)
 aev = get(paste(plat, "PATH2PROBE", sep=""))
 na.action(unique(unlist(mget(tags, aev, ifnotfound=NA))))
}
