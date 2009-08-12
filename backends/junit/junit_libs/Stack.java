package junit_libs;

public interface Stack<T> {
	public void push(T obj);

	public T pop() throws StackException;

	public T top() throws StackException;

	public boolean isEmpty();
}
