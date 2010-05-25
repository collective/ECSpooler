package junit_libs;

import java.util.List;

public interface InterfaceWGraph {	
	boolean getIsDir();
	boolean addNode(String label);
	boolean removeNode(String label);
	boolean addEdge(String src, String dest, double weight);	
	boolean removeEdge(String orig, String dest);
	boolean NodeIn(String label);	
	boolean EdgeIn(String src, String dest);
	List<String> adjacent(String label);
	int outDegree(String label);
	int inDegree(String label);
	int getNumNodes();
	int getNumEdges();
}
