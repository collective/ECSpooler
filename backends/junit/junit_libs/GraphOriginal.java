/**
 * Diese Zeile ist fuer jede *.java-Datei noetig, damit der Compiler weiss, wo er zu suchen hat.
 */
package junit_libs;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

/**
 * 
 * @coauthor christian
 * @modified 23.04.2009, chbauman:
 * 		Interne Klassen auf 'protected' gesetzt.
 * 		Interne Zustaende der internen Klassen 'public' gesetzt.
 * 		Kommentare und Formatierung.
 */
public class GraphOriginal {

	public final static int WHITE = 0;

	public final static int GRAY = 1;

	public final static int BLACK = 2;

	/**
	 * Nutze statt des Modifiers 'default' 'protected', so dass Kindklassen von
	 * GraphOriginal auf Edge zugreifen koennen.
	 */
	protected class Edge {
		// Da in der Kindklasse auf diese Werte zugegriffen werden koennte, setze sie 'public'.
		public int dest, cost;

		public Edge(int d, int c) {
			dest = d;
			cost = c;
		}

		public String toString() {
			return new String(dest + "(" + cost + ")");
		}
	}
	
	/**
	 * @see Edge Kommentar
	 */
	protected class BFSItem {
		// Da in der Kindklasse auf diese Werte zugegriffen werden koennte, setze sie 'public'.
		public int color, distance, prev;

		public BFSItem(int c, int d, int p) {
			color = c;
			distance = d;
			prev = p;
		}
	}

	/**
	 * @see Edge Kommentar
	 */
	protected class DFSItem {
		// Da in der Kindklasse auf diese Werte zugegriffen werden koennte, setze sie 'public'.
		public int color, prev, d, f;

		public DFSItem() {
			color = WHITE;
			prev = 0;
			d = f = 0;
		}
	}

	protected List<String> labels;

	/*
	 * Lege nodes als eine List von List mit Edges als Füllung an.
	 */
	protected List<List<Edge>> nodes;

	public GraphOriginal() {
		// Okay, labels ist eine List - ArrayList ist die Implementierungsklasse in unserem Fall.
		labels = new ArrayList<String>();
		
		/*
		 * nodes ist eine List<List<...>>.
		 * Wir definieren nun, dass eine ArrayList unbestimmte List-Instanzen mit Edge-Objekten haelt.
		 * Dadurch koennen wir in unsere ArrayList jede erdenkliche andere List reinstecken.
		 * 
		 * Wuerden wir ArrayList<ArrayList<...>> sagen, waere das zu restriktiv, da nodes oben als
		 * "generelle List-Instanz von einer generellen List-Instanz von Edge-Objekten" ist.
		 * nodes sieht ja nach aussen nur als List<List<...>> aus, weswegen wir 
		 * 
		 * nodes.add(new LinkedList<Edge>());
		 * nodes.add(new ArrayList<Edge>());
		 * 
		 * aufrufen koennten. Die erste zuweisung wird dann aber fehlschlagen, da man eine LinkedList
		 * nicht als ArrayList ausgeben kann.
		 */
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

	public String toString() {
		String s = "";
		for (String l : labels) {
			s += l + "(" + getNodeID(l) + ")" + " -> "
					+ nodes.get(getNodeID(l)) + "\n";
		}
		return s;
	}
}