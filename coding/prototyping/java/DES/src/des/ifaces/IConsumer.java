package des.ifaces;

public interface IConsumer<T> {

	 void inputArrival(T e, IClock c);
	 boolean isReady();
}
