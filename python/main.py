from gendata import ptime, pfac, afac
from matching import bipartiteMatch

# Step 1: Make sure every watcher watches someone
watchers = pfac + afac
targets = ptime + afac
graph1 = {}
for target in targets:
	graph1[target] = []
	for watcher in watchers:
		if watcher.pos[0] == "A" and target.pos[0] == "A": continue
		if watcher.canWatch(target):
			graph1[target].append(watcher)

res1 = bipartiteMatch(graph1)[0]
unvisited_people = [targ for targ in targets if targ not in res1.values()]
unused_watchers = [watcher for watcher in watchers if watcher not in res1.keys()]
assert len(unused_watchers) == 0, "aww"

# Match any unvisited to watchers
graph2 = {}
for watcher in watchers:
	graph2[watcher] = []
	for target in unvisited_people:
		if watcher.pos[0] == "A" and target.pos[0] == "A": continue
		if watcher.canWatch(target):
			graph2[watcher].append(target)

res2 = bipartiteMatch(graph2)[0]

final = []
for watcher in res1:
	target = res1[watcher]
	final.append( (watcher, target) )
for target in res2:
	watcher = res2[target]
	final.append( (watcher, target) )

final.sort(key = lambda tup : tup[1].pos[1] + tup[1].name) # this is a terrible key
# print r"\begin{tabular}{lll}"
for k,v in final:
	print v,"\t&\t",k,"\t&\t",k.canWatch(v),r"\\"
# print r"\end{tabular}"
