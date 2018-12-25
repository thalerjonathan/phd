package org.evensen.util;

/**
 * Immutable class for generic pairs.
 * 
 * @author Pelle Evensen
 * @author Anders Moberg
 */
public class Pair<A, B> {
	/**
	 * The first element of the pair.
	 */
	public final A first;

	/**
	 * The second element of the pair.
	 */
	public final B second;

	/**
	 * Makes a new pair of type <A,B>.
	 * 
	 * @param first
	 *            the first element of the pair.
	 * @param second
	 *            the second element of the pair.
	 */
	public Pair(A first, B second) {
		this.first = first;
		this.second = second;
	}

	@Override
	public boolean equals(Object o) {
		return o instanceof Pair && ((Pair<?, ?>) o).first.equals(first)
				&& ((Pair<?, ?>) o).second.equals(second);
	}

	@Override
	public int hashCode() {
		return first.hashCode() * 13579 + second.hashCode() * 23456789;
	}

	@Override
	public String toString() {
		return "<" + first.toString() + "," + second.toString() + ">";
	}
}
