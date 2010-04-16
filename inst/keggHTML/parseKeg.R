
parseKeg = function(lines=readLines(
   url("ftp://ftp.genome.jp/pub/kegg/brite/ko/ko00001.keg"))) {
 # the k00001.keg that encodes the KEGG orthology
 # must be parsed to get the hierarchy
 # assume you are given the readLines on the .keg file
 # keep only A:C starters
 inichar = substr(lines, 1, 1)
 kp = which(inichar %in% c("A", "B", "C"))
 orth = lines[kp]
 kill = which(nchar(orth) == 1)
 orth = orth[-kill]
 # now have to chop up by C within B, B within A
 inichar = substr(orth, 1, 1)
 Aind = which(inichar=="A")
 AA = c(Aind, length(orth)+1)
 AG = rep(1:length(Aind), diff(AA))
 sorth = split(orth, AG)
 Arecs = sapply(sorth, function(x)x[1])
 borth = lapply(sorth, function(x) x[-1])
 # get the cruft off the A records
 getTag = function(x) sub(".*([0-9]{5}).*","\\1", x)
 Atags = getTag(Arecs)
 Anames = sub("<.*", "",sub("A.*[0-9]\ (.*)", "\\1", Arecs))
 names(borth) = Anames
 # chop up the list elements by B
 Binds = lapply( borth, function(x) which(substr(x,1,1)=="B") )
 BB = BG = sob = Brecs = Bnames = Btags = Ctags = list()
 for (i in 1:length(Binds)) {
     BB[[i]]=c(Binds[[i]], length(borth[[i]])+1)
     BG[[i]]=rep(1:length(Binds[[i]]), diff(BB[[i]]))
     sob[[i]] = split(borth[[i]], BG[[i]])
     Brecs[[i]] = sapply(sob[[i]], function(x)x[1])
     Btags[[i]] = sapply(Brecs[[i]], getTag)
     Bnames[[i]] = sub("<.*", "",sub("B.*[0-9]\ (.*)", "\\1", Brecs[[i]]))
     sob[[i]] = lapply(sob[[i]], function(x)x[-1])
     Ctags[[i]] = sapply(sob[[i]], getTag)
     sob[[i]] = lapply(sob[[i]], function(x) sub(".*[0-9]{5} (.*) \\[PATH.*$", "\\1", x))
# guess what -- not all C level entries have a [PATH: ....!
     sob[[i]] = lapply(sob[[i]], function(x) sub(".*[0-9]{5} (.*)$", "\\1", x))
     names(sob[[i]]) = Bnames[[i]]
     names(Ctags[[i]]) = Bnames[[i]]
     }
 names(sob) = Anames
 names(Ctags) = Anames
 list(fullList=sob, Anames=Anames, Atags=Atags, Bnames=Bnames, Btags=Btags, Ctags=Ctags)
 }

#kp2graph = function(kp) {
# require(graph)
# alln = 
