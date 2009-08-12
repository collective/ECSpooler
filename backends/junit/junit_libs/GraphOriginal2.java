package junit_libs;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class GraphOriginal2 {

	public final static int WHITE = 0;

	public final static int GRAY = 1;

	public final static int BLACK = 2;

	
	protected class Edge {
		
		public int dest, cost;

		public Edge(int d, int c) {
			dest = d;
			cost = c;
		}

		public String toString() {
			return new String(dest + "(" + cost + ")");
		}
	}

	protected class BFSItem {
		public int color, distance, prev;

		public BFSItem(int c, int d, int p) {
			color = c;
			distance = d;
			prev = p;
		}
	}

	protected class DFSItem {
		public int color, prev, d, f;

		public DFSItem() {
			color = WHITE;
			prev = 0;
			d = f = 0;
		}
	}

	protected List<String> labels;

	protected List<List<Edge>> nodes;

	public GraphOriginal2() {
		labels = new ArrayList<String>();
		nodes = new ArrayList<List<Edge>>();
	}

	public static void main(String[] args) {

	}

	public void addNode(String label) {
		if (labels.contains(label))
			throw new RuntimeException("Node already defined");
		nodes.add(new ArrayList<Edge>());
		int idx = nodes.size() - 1;
		labels.add(idx, label);
	}

	public int getNodeID(String label) {
		int i = labels.indexOf(label);
		if (i == -1)
			throw new RuntimeException("no such element");
		return i;
	}

	public void addEdge(String src, String dest, int cost) {
		nodes.get(getNodeID(src)).add((new Edge(getNodeID(dest), cost)));
	}

	public void searchBreadthFirst(int start) {
		int i;
		BFSItem[] table = new BFSItem[labels.size()];
		Queue<Integer> queue = new LinkedList<Integer>();

		for (i = 0; i < table.length; i++)
			table[i] = new BFSItem(WHITE, Integer.MAX_VALUE, -1);

		table[start].color = GRAY;
		table[start].distance = 0;
		queue.offer(start);
		while (!queue.isEmpty()) {
			int u = queue.poll();
			for (Edge e : nodes.get(u)) {
				if (table[e.dest].color == WHITE) {
					table[e.dest].color = GRAY;
					table[e.dest].distance = table[u].distance + 1;
					table[e.dest].prev = u;
					queue.offer(e.dest);
				}
			}
			table[u].color = BLACK;
			System.out.println("vertex #" + u + ": " + table[u].distance);
		}
	}

	int time;

	public void searchDepthFirst() {
		time = 0;
		int i = 0;
		DFSItem[] table = new DFSItem[labels.size()];

		for (i = 0; i < table.length; i++)
			table[i] = new DFSItem();

		for (i = 0; i < table.length; i++)
			if (table[i].color == WHITE)
				dfsVisit(i, table);
	}

	private void dfsVisit(int nr, DFSItem[] table) {
		table[nr].color = GRAY;
		table[nr].d = ++time;
		for (Edge e : nodes.get(nr)) {
			if (table[e.dest].color == WHITE) {
				table[e.dest].prev = nr;
				dfsVisit(e.dest, table);
			}
		}
		table[nr].color = BLACK;
		table[nr].f = ++time;

		System.out.println("vertex #" + nr + ": " + table[nr].d + ", "
				+ table[nr].f);
	}

	public List<String> adjacent(String node) {
		// TODO Bitte hier Ihren Code einfuegen
		List<String> erg = new LinkedList<String>();
		int nodeID = getNodeID(node);
		for (Edge edge : nodes.get(nodeID)) {
			erg.add(labels.get(edge.dest));
		}
		return erg;
	}

	public List<String> nodes() {
		// TODO Bitte hier Ihren Code einfuegen
		return labels;
	}

	public boolean edgeIn(String src, String dest) {
		// TODO Bitte hier Ihren Code einfuegen
		for (Edge e : nodes.get(getNodeID(src))) {
			if (e.dest == getNodeID(dest))
				return true;
		}
		return false;
	}

	public int weight(String src, String dest) {
		// TODO Bitte hier Ihren Code einfuegen
		for (Edge e : nodes.get(getNodeID(src))) {
			if (e.dest == getNodeID(dest))
				return e.cost;
		}
		throw new RuntimeException("no such edge");
	}

	public String toString() {
		String s = "";
		for (String l : labels) {
			s += l + "(" + getNodeID(l) + ")" + " -> "
					+ nodes.get(getNodeID(l)) + "\n";
		}
		return s;
	}
}
