collapse.clade.ce2 <- function(ChromEvol, node, name="collapsed", number=""){
	
	ceN <- ChromEvol
	#find all tips descending from this node
	tipsList <- list()
for (i in 1:length(node)){
	tips <- descendants(ChromEvol[[3]],node[i])
	tipsList[[i]] <- ChromEvol[[3]]$tip.label[tips]
	}
	cat("The clades/tips to be cut from the tree:\n")
	print(tipsList)
for (i in 1:length(node)){	
	tips <- match(tipsList[[i]], ChromEvol[[3]]$tip.label)
	NODE <- noi(ChromEvol[[3]],tipsList[[i]])
	T <- tips[-1]
	#find all nodes descending from node
	nodes <- c(NODE,descendants(ChromEvol[[3]],NODE, type="i"))
	
	N <- nodes - length(ChromEvol[[3]]$tip.label)
	E <- which.edge(ChromEvol[[3]], T)
	E <- c(E[1]-1, E)

	ceN[[1]][[1]] <- ChromEvol[[1]][[1]][-N,]
	ceN[[1]][[2]] <- ChromEvol[[1]][[2]][-T,]
	ceN[[1]][[3]] <- ChromEvol[[1]][[3]][-N]

	ceN[[2]][[2]] <- ChromEvol[[2]][[2]][-E,]
	if (number[1] != "")
		number2 <- paste("-",number[i], sep="")
	else
		number2=""
	name2 <- paste(name[i], " (", length(tips), " tips)",number2, sep="")
	ChromEvol[[3]]$tip.label[tips[1]] <- name2
	ceN[[3]] <- drop.tip(ChromEvol[[3]], T)
	
	ceN$collapse <- rbind(ceN$collapse, ChromEvol[[1]][[1]][node[i]-length(ChromEvol[[3]]$tip.label),])
	dimnames(ceN$collapse)[[1]][length(ceN$collapse[,1])] <- name2
	ChromEvol <- ceN
	
	}
	ceN
}