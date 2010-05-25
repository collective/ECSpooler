package junit_libs; 

import java.util.*;

public class WeightedGraph implements InterfaceWGraph {
	protected class Edge {

		private GraphNode destNode;
		private double weight;

		protected Edge(GraphNode d, double c) {
			destNode = d;
			weight = c;
		}

		public GraphNode getDestNode() {
			return destNode;
		}

		public double getWeight() {
			return weight;
		}

		public String getLabel() {
			return destNode.getLabel();
		}

		public String toString() {
			return new String(destNode.getLabel() + "(" + weight + ")");
		}
	}

	protected class GraphNode {

		private String label;
		private List<Edge> edges = new LinkedList<Edge>();

		public List<Edge> getEdges() {
			return edges;
		}

		protected GraphNode(String label) {
			this.label = label;
			this.edges = new LinkedList<Edge>();
		}

		public String toString() {
			String result = "\n" + this.label + " -> ";
			for (Edge n : edges)
				result += n + " ";
			return result;
		}

		public String getLabel() {
			return label;
		}

	}

	private LinkedList<GraphNode> nodes;
	private int numNodes; // Anzal Knoten
	private int numEdges; // Anzahl Kanten
	private boolean dir;

	public WeightedGraph(boolean dir) {
		nodes = new LinkedList<GraphNode>();
		numNodes = 0;
		numEdges = 0;
		this.dir = dir;
	}

	public boolean getIsDir() {
		return dir;
	}

	public int getNumNodes() {
		return numNodes;
	}

	public List<GraphNode> getNodes() {
		return nodes;
	}

	public int getNumEdges() {
		return numEdges;
	}

	/*protected Iterator<GraphNode> iterator() {
		return nodes.iterator();
	}*/

	public String toString() {
		String erg = "";
		for (GraphNode n : nodes) {
			erg += n;
		}
		return erg;
	}

	public boolean addNode(String label) {
		if (!this.NodeIn(label)) {
			nodes.add(new GraphNode(label));
			numNodes++;
			return true;
		}
		return false;
	}

	protected GraphNode getNode(String label) {
		for (GraphNode n : nodes)
			if (n.label.equals(label))
				return n;
		// throw new NoSuchElementException("Knoten " + label + " nicht gefunden");
		return null;
	}
	protected Edge getEdge(GraphNode node, String label) {
        for (Edge e : node.getEdges()) {
            if (e.getLabel().equals(label)) {
                return e;
            }
        }
        return null;
    }
	public boolean NodeIn(String label) {
        return getNode(label) != null;
    }

	public boolean EdgeIn(String src, String dest) {
        if (NodeIn(src)) {
            return getEdge(getNode(src), dest) != null;
        } else {
            return false;
        }
    }

	public List<String> adjacent(String label) {
		List<String> result = new LinkedList<String>();
		if (!NodeIn(label)) {
            return result;
        }
		GraphNode n = this.getNode(label);
		for (Edge es : n.edges)
				result.add(es.destNode.getLabel());
		return result;
	}

	public int outDegree(String label) {
		if (!NodeIn(label)) {
            return -99;
        }
		GraphNode n = this.getNode(label);		
		return n.edges.size();
	}

	public int inDegree(String label) {
		if (!NodeIn(label)) {
			return -99;
		}
		int sum = 0;
		for (GraphNode n : nodes) {
			if (EdgeIn(n.getLabel(), label)) {
				sum++;
			}
		}
		return sum;
	}
	/**
     * Entfernt eine Kante.
     *
     * @param orig Name des ausgehenden Knotens
     * @param dest Name des Zielknotens
     * @return <tt>true</tt> wenn Knoten erfolgreich entfernt
     */
	public boolean removeEdge(String orig, String dest) {
		if(EdgeIn(orig, dest)){
			getNode(orig).getEdges().remove(getEdge(getNode(orig), dest));
			if(!getIsDir()){
				getNode(dest).getEdges().remove(getEdge(getNode(dest), orig));
			}
			numEdges--;
			return true;
		}
		return false;
	}
	
	
	public boolean removeNode(String label) {
		if (!NodeIn(label))
			return false;
		for (GraphNode n : nodes) {
			if (!n.getLabel().equals(label)) {
				// "zu label hinfuehrende Kanten"
				removeEdge(n.getLabel(), label);
			}

			if (dir) {
				// Versuchen alle (outDegree) Kanten zu loeschen, auch wenn
				// man nicht weiss, ob die Kante vorhanden ist.
				// Jedes mal zu pruefen, ob die Kante vorhanden ist, ist
				// mehr Aufwand als gleich versuchen zu loeschen.
				removeEdge(label, n.getLabel());
			}
		}
		nodes.remove(getNode(label));
		numNodes--;
		return true;
	}

	public boolean addEdge(String src, String dest, double w) {
        if(EdgeIn(src,dest)){
        	removeEdge(src,dest);
        	addEdge(src,dest,w);
        	return false;
        }
		if (NodeIn(src) && NodeIn(dest) && !EdgeIn(src, dest)) {
            getNode(src).getEdges().add(new Edge(getNode(dest), w));

            if (!dir) {
                getNode(dest).getEdges().add(new Edge(getNode(src), w));
            }

            numEdges++;
            return true;
        } else {
            return false;
        }
    }
}
