#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 9           #
#      2017-11362 박건도     #
#                           #
#############################


# problem_B
class Node:
    def __init__(self, name):
        self.name = name

    def getName(self):
        return self.name

    def __str__(self):
        return self.name


class Edge:
    def __init__(self, src, dest):
        self.src = src
        self.dest = dest

    def getSource(self):
        return self.src

    def getDestination(self):
        return self.dest

    def __str__(self):
        return self.src.getName() + "->" + self.src.getDestination()


class Digraph:
    def __init__(self):
        self.nodes = []
        self.edges = {}

    def addNode(self, node):
        if node in self.nodes:
            raise ValueError("Duplication node")
        else:
            self.nodes.append(node)
            self.edges[node] = []

    def addEdge(self, edge):
        src = edge.getSource()
        dest = edge.getDestination()
        if not (src in self.nodes and dest in self.nodes):
            raise ValueError("Node not in graph")
        self.edges[src].append(dest)

    def childrenOf(self, node):
        return self.edges[node]

    def hasNode(self, node):
        return node in self.nodes

    def __str__(self):
        result = ""
        for src in self.nodes:
            for dest in self.edges[src]:
                result += src.getName() + "->" + dest.getName() + "\n"
        return result[:-1]


def diff_norm(d1, d2):
    n = 0
    result = 0
    for i in d1.keys():
        result += (d1[i] - d2[i]) ** 2
        n += 1
    return result / n


def pagerank(graph, d, tol=1e-6):
    D, T = {}, {}
    for src in graph.nodes:
        for dest in graph.childrenOf(src):
            srcName = src.getName()
            destName = dest.getName()
            if srcName in D:
                D[srcName] += 1
            else:
                D[srcName] = 1
            if (destName, srcName) in T:
                T[(destName, srcName)] += 1
            else:
                T[(destName, srcName)] = 1
    for (dest, src) in T.keys():
        T[(dest, src)] /= D[dest]
    # iteration method
    # p_(n+1) = d * T * p_(n) + (1 - d) * e
    p, p_old = {}, {}
    for node in D:
        p[node], p_old[node] = 1, 0
    while diff_norm(p, p_old) > tol:
        p_old = {node: 0 for node in p_old}
        for (i, j) in T.keys():
            p_old[i] += d * T[(i, j)] * p[j]
        for i in p_old.keys():
            p_old[i] = d * p_old[i] + (1 - d)
        p, p_old = p_old, p
    return sorted([(i, p[i]) for i in p], key=lambda x: -x[1])


if __name__ == "__main__":
    g = Digraph()
    nodes = [Node(str(name)) for name in range(7)]
    for n in nodes:
        g.addNode(n)
    g.addEdge(Edge(nodes[0], nodes[1]))
    g.addEdge(Edge(nodes[0], nodes[2]))
    g.addEdge(Edge(nodes[0], nodes[3]))
    g.addEdge(Edge(nodes[0], nodes[4]))
    g.addEdge(Edge(nodes[0], nodes[5]))
    g.addEdge(Edge(nodes[1], nodes[0]))
    g.addEdge(Edge(nodes[1], nodes[2]))
    g.addEdge(Edge(nodes[1], nodes[5]))
    g.addEdge(Edge(nodes[2], nodes[3]))
    g.addEdge(Edge(nodes[3], nodes[0]))
    g.addEdge(Edge(nodes[4], nodes[0]))
    g.addEdge(Edge(nodes[4], nodes[2]))
    g.addEdge(Edge(nodes[4], nodes[3]))
    g.addEdge(Edge(nodes[4], nodes[5]))
    g.addEdge(Edge(nodes[5], nodes[1]))
    g.addEdge(Edge(nodes[5], nodes[6]))
    g.addEdge(Edge(nodes[6], nodes[5]))
    print(pagerank(g, 1))
    print(pagerank(g, 0.5))
