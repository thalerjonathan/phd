package barter;

import org.evensen.util.Pair;

import ec.util.MersenneTwisterFast;

/**
 * Util class for the barter economy.
 * 
 * @Immutable This class is immutable.
 */
public final class Util {

	public static final double ALMOST_ONE = Math.nextAfter(1.0, -1.0); 

	// Private constructor to prevent instantiation.
	private Util() { }
	
	/**
	 * Creates a random permutation of indices 0, ..., size - 1.
     *
     * @pre rand != null
     *
	 * @param size The size of the permutation
	 * @param rand The random number generator to use.
	 * @return An array containing the indices 0, ..., size - 1 in random order.
	 */
    public static int[] randomPermutation(int size, MersenneTwisterFast rand) {
        int[] arr = new int[size];
        for (int i = 0; i < size; i++) {
            arr[i] = i;
        }

        for (int i = size - 1; i > 0; i--) {
            int r = rand.nextInt(i + 1);
            int tmp = arr[r];
            arr[r] = arr[i];
            arr[i] = tmp;
        }

        return arr;
    }

    /**
     * Return a random pair {i,j} of two integers, where i != j.
     * @param range	Values for i and j are in the range [0..range)
     * @return Pari of two integers.
     */
    public static Pair<Integer, Integer> randomPair(int range, MersenneTwisterFast rand) {
		int i = rand.nextInt(range);
		int j;
		do {
			j = rand.nextInt(range);
		} while (i == j);

		return new Pair<Integer, Integer>(i,j);
    }

	static double distance(double[] a, double[] b) {
		double sum = 0.0;
		int len = Math.max(a.length, b.length);

		for(int i = 0; i < len; i++) {
			sum += Math.pow(a[i] - b[i], 2.0);
		}

		return Math.sqrt(sum);
	}

	public static double threshold(double min, double v, double max) {
		return Math.max(min, Math.min(max, v));
	}

	/**
	 * Shuffle elements <code>begin ... end - 1</code> by using any element in <code>arr</code>
	 * @param <T>
	 * @param arr
	 * @param begin
	 * @param end
	 * @param random
	 */
	public static <T> void shuffleHead(T[] arr, int size, MersenneTwisterFast random) {
		for(int i = 0; i < size; i++) {
			int r = random.nextInt(arr.length - i);
			T tmp = arr[r];
			arr[r] = arr[i];
			arr[i] = tmp;
		}
	}
}