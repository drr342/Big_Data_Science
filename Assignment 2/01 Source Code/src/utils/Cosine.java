package utils;

public class Cosine implements Similarity {

	@Override
	public double distance (double[] x, double[] y) {
		if (x.length != y.length)
			return Double.NaN;
		double d = 0.0;
		for (int i = 0; i < x.length; i++) {
			d += x[i] * y[i];
		}
		return d / (Vectors.norm(x) * Vectors.norm(y));
	}

}
