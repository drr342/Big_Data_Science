package utils;

public final class Vectors {
	
	private Vectors () {
		throw new RuntimeException();
	}
	
	public static double[] sumArrays (double[] x, double[] y) {
		if (x.length != y.length) return null;
		double sum[] = new double[x.length];
		for (int i = 0; i < x.length; i++) {
			sum[i] = x[i] + y[i];
		}
		return sum;
	}
	
	public static double norm (double[] v) {
		double n = 0.0;
		for (int i = 0; i < v.length; i++) {
			n += v[i] * v[i];
		}
		return Math.sqrt(n);
	}
	
	public static double[] average (double[] x, double n) {
		double[] average = new double[x.length];
		for (int i = 0; i < x.length; i++) {
			average[i] = x[i] / n;
		}
		return average;
	}
	
	public static double[] normalize (double[] x) {
		return average(x, norm(x));
	}

}
