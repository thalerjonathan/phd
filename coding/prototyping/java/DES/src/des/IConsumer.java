package des;

public interface IConsumer<T> {

	 void inputArrival(T e, IClock c);
	 boolean isReady();
}
