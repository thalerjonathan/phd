package des;

public interface IProducer<T> {

	T getNext();
	boolean hasNext();
}
