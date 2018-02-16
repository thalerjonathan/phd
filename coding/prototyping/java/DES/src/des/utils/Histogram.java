package des.utils;

public class Histogram {

	private double binSize;
	private long[] bins;
	
	private double min;
	private long totalItems;
	
	public Histogram(int n, double min, double max) {
		this.bins = new long[n];
		this.binSize = (max - min) / n;
		this.min = min;
	}
	
	public void add(double d) {
		int bin = 0;
		double binValue = this.min;
				
		for (int i = 0; i < this.bins.length; ++i) {
			binValue += binSize;
			
			if (d <= binValue) {
				bin = i;
				break;
			}
		}
		
		this.bins[bin]++;
		this.totalItems++;
	}
	
	public double getMean() {
		double mean = 0;
		double binValue = this.min;
		
		for (int i = 0; i < this.bins.length; ++i) {
			double meanBinValue = binValue + (binSize * 0.5);
			double weidghtedMeanBinValue = meanBinValue * this.bins[i];
			
			mean += weidghtedMeanBinValue;
			binValue += this.binSize;
		}
		
		return mean / this.totalItems;
	}
	
	public long getTotalItems() {
		return this.totalItems;
	}
	
	@Override
	public String toString() {
		String str = "";
		double binValue = this.min;
		
		for (int i = 0; i < this.bins.length; ++i) {
			str += "Bin " + i + " " + binValue + " - " + (binValue + binSize) + ": " + this.bins[i] + "\n";	
			binValue += binSize;
		}
		
		return str;
	}
}
