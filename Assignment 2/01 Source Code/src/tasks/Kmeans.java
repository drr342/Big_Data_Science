package tasks;

import java.util.Random;

import utils.Clustering;
import utils.Cosine;
import utils.Similarity;
import utils.Vectors;

public class Kmeans  extends Clustering {

	public Kmeans(double[][] matrix, Similarity measure, int k) {
		super(matrix, measure, k);
	}

	@Override
	protected void initializeCentroids () {
		Random rand = new Random();
		for (int i = 0; i < k; i++) {
			centroids[i] = matrix[rand.nextInt(matrix.length)];
		}
		if (measure instanceof Cosine) {
			for (int i = 0; i < k; i++) {
				centroids[i] = Vectors.normalize(centroids[i]);
			}
		}
	}

}
