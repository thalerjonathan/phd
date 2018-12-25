package barter;

public interface OptionHandler<O,S> {
	public void handleOption(O option, S state);
}
