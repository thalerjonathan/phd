package barter.test;

import static org.junit.Assert.assertTrue;

import org.evensen.util.Statistics;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import ec.util.MersenneTwisterFast;

import barter.Util;
//import barter.Util;

public class UtilTest {
	private MersenneTwisterFast rand;
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		System.out.println("setUpBeforeClass");
	}

	@Before
	public void setUp() throws Exception {
		rand = new MersenneTwisterFast(12);
		System.out.println("setUp");
	}

	private static int fac(int n) {
		return n > 0 ? fac(n - 1) * n : 1;
	}
	
	private static boolean isPermutation(int[] v) {
		int[] hist = new int[v.length];
		
		for(int e : v) {
			if(!(e > v.length || e < 0 || hist[e] != 0)) {
				hist[e] = 1;
			} else {
				return false;
			}
		}
		
		return true;
	}
	
	private static int permIndex(int[] p) {
	    int index = 0;
	    
	    for(int i = 1; i < p.length; i++) {
	        int ball = p[i - 1];
	        for(int j = i; j < p.length; j++) {
	            if (ball > p[j]) {
	            	index ++;
	            }
	        }

	        index *= p.length - i;
	    }

	    return index;
	}
	
	@Test
	public void checkIsPermutation() {
		for(int i = 1; i < 1000; i += 30) {
			int[] p = Util.randomPermutation(i * (i + 1) - 1, rand);
			assert(isPermutation(p));
		}
	}
	
	@Test
    public void checkPermutationDistribution() {
		// Check that all permutation elements seem to be distributed evenly;
		rand.nextLong();
		rand.nextLong();
		for(int permSize = 5; permSize < 11; permSize++) {
			double min = Double.MAX_VALUE;
			double max = Double.MIN_VALUE;
			int fac = fac(permSize);
			double mean = 1; 

			for(int tests = 0; tests <= Math.max(3, 1000 / fac); tests++) {
				int[] permHist = new int[fac];
				
				for(int i = 0; i < fac; i++) {
					int p = permIndex(Util.randomPermutation(permSize, rand));
					permHist[p]++;
				}
				double variance = Statistics.variance(permHist);
				min = Math.min(min, Math.abs(variance - mean));
				max = Math.max(max, Math.abs(variance - mean));
			}
//			System.out.println("Min: " + min + "\tMax: " + max + "\tDiff: " +
//					(max - min));
			assertTrue(min < Math.pow(2.5, -(permSize - 3)));
			assertTrue(max > Math.pow(2.5, -(permSize - 2)));
		}

	}
		

	@After
	public void tearDown() throws Exception {
		rand = null;
		System.out.println("tearDown");
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		System.out.println("tearDownAfterClass");
	}
}