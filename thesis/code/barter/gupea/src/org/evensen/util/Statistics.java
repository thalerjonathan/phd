package org.evensen.util;

import java.util.List;

public class Statistics {
	public static double mean(Double[] v) {
		return mean(toDoubleArray(v));
	}
	
	public static double mean(double[] v) {
		if(v.length <= 0) {
			throw new IllegalArgumentException("Mean of an empty array is undefined.");
		}
		double sum = 0.0;
		
		for(double e : v) {
			sum += e;
		}
		
		return sum / v.length;
	}
	
	public static double mean(List<Double> v) {
		return mean(v.toArray(new Double[0]));
	}
	
	public static double[] toDoubleArray(Double[] v) {
		double[] v2 = new double[v.length];
		int i = 0;
		for(Double e : v) {
			v2[i++] = e;
		}
		
		return v2;
	}

	public static double[] toDoubleArray(int[] v) {
		double[] v2 = new double[v.length];
		int i = 0;
		for(int e : v) {
			v2[i++] = e;
		}
		
		return v2;
	}

	public static double variance(int[] v) {
		double[] v2 = toDoubleArray(v);
		return variance(v2, mean(v2));
	}

	public static double variance(double[] v) {
		return variance(v, mean(v));
	}

	public static double variance(List<Double> v) {
		return variance(toDoubleArray(v.toArray(new Double[0])));
	}
	
	public static double variance(double[] v, double mean) {
		double var = 0.0;
		
		for(Double e : v) {
			var += Math.pow(e - mean, 2.0);
		}
		
		return var / (v.length - 1);
	}
	
}
