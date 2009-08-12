package junit_libs;

public class Node<T> {
    // Instance variables:
    private T element;
    private Node<T> next;
    // Simple constructors:
    public Node() {
    	this(null, null);
    }
    public Node(T e, Node<T> n) {
		element = e;
		next = n;
    }
    // Accessor methods:
    public T getElement() {
    	return element; 
    }
    public Node<T> getNext() { 
    	return next;
    }
    // Modifier methods:
    public void setElement(T newElem) { 
    	element = newElem; 
    }
    public void setNext(Node<T> newNext) {
    	next = newNext; 
    }
    
    public boolean isList (){
    	if (next != null)
    		return next.isList();
    	else return true;
    }

    // Klassenvariable (Defaultwerte)

    static String listStart = "["; // start of a list in printed representation
    static String listEnd = "]"; // end of a list in printed representation 
    static String elemSeparator = ", "; // separator of elements of a list     

    public String printList (){
	return listStart + printElems() + listEnd;
    }
 
    public String printElems(){
	return (((element instanceof Node) && (((Node<?>)element).isList()))
		? ((Node<?>)element).printList() : element.toString())
	    + ((next != null) ? elemSeparator + next.printElems() : "");
    }

    public String toString(){
		return element.toString()
		    + ((next != null) ? elemSeparator + next.toString() : "");
    }


    public int length(){
    	return ((next == null) ? 1 : 1 + next.length());
    }



}
