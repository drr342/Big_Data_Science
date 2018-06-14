package tasks;

import org.ejml.simple.SimpleMatrix;
import org.ejml.simple.SimpleSVD;

public final class SVD {
	
	private SVD () {
		throw new RuntimeException();
	}

	@SuppressWarnings("rawtypes")
	public static double[][] reduce (double[][] matrix, int dimensions) {
		SimpleMatrix M = new SimpleMatrix(matrix);
		SimpleSVD s = M.svd();
		SimpleMatrix U = s.getU().extractMatrix(0, SimpleMatrix.END, 0, dimensions);
		SimpleMatrix W = s.getW().extractMatrix(0, dimensions, 0, dimensions);
		SimpleMatrix V = s.getV().extractMatrix(0, dimensions, 0, dimensions);
		SimpleMatrix reduced = U.mult(W).mult(V.transpose());
		
		double[][] output = new double[reduced.numRows()][reduced.numCols()];
		for (int i = 0; i < reduced.numRows(); i++) {
			for (int j = 0; j < reduced.numCols(); j++) {
				output[i][j] = reduced.get(i, j);
			}
		}
		
		return output;
	}

}
