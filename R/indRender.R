
indRender = function(klike, from=nodes(klike)[1], indent="  ") {
 # check for KO attributes
 ndd = names(unlist(nodeDataDefaults(klike)))
 if (!("depth" %in% ndd)) stop("must have nodeData attribute 'depth'")
 allt = nodes(klike)
 targs = unlist(lapply(acc(klike, from), names))
 #
 # bug in subgraph necessitates reconstruction of nodeData
 #
 denew = nodeData(klike, targs, "depth")
 tanew = nodeData(klike, targs, "tag")
 gg = subGraph( targs, klike )
 nodeDataDefaults(gg) = list(depth=0, tag="NONE")
 nodeData(gg, targs, "depth") = denew
 nodeData(gg, targs, "tag") = tanew
 de = unlist(nodeData(gg,,"depth"))
 # now render with cat
 allt = nodes(gg) 
 for (i in 1:length(allt)) {
  for (j in 1:de[i])  cat(indent)
  cat(allt[i], "\n")
  }
invisible(NULL)
}

