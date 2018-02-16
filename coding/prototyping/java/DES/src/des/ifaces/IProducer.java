package des.ifaces;

public interface IProducer<T> {

	T getNext();
	boolean hasNext();
}
