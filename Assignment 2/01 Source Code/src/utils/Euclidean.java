package utils;

public class Euclidean implements Similarity {

	@Override
	public double distance (double[] x, double[] y) {
		if (x.length != y.length)
			return Double.NaN;
//		double[] normX = Vectors.normalize(x);
//		double[] normY = Vectors.normalize(y);
		double[] normX = x;
		double[] normY = y;
		double d = 0.0;
		for (int i = 0; i < x.length; i++) {
			d += (normX[i] - normY[i]) * (normX[i] - normY[i]);
		}
		return d;
	}

}
