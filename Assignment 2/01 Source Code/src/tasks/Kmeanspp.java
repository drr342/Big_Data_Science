package tasks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import utils.Clustering;
import utils.Euclidean;
import utils.Similarity;
import utils.Vectors;

public class Kmeanspp extends Clustering {

	public Kmeanspp(double[][] matrix, Similarity measure, int k) {
		super(matrix, measure, k);
	}

	@Override
	protected void initializeCentroids() {
		Random rand = new Random();
		centroids[0] = Vectors.normalize(matrix[rand.nextInt(matrix.length)]);
		List<Double> distances = new ArrayList<>();
		int n = 1;
		for (int index = 1; index < k; index++) {
			distances.clear();
			for (int i = 0; i < matrix.length; i++) {
				double min = Double.POSITIVE_INFINITY;
				for (int j = 0; j < n; j++) {
					min = Math.pow(Math.min(min, (new Euclidean()).distance(Vectors.normalize(matrix[i]), centroids[j])), 2);
				}
				distances.add(min);
			}
			double sum = distances.stream().mapToDouble(Double::doubleValue).sum();
			List<Integer> probabilities = new ArrayList<>();
			for (int i = 0; i < distances.size(); i++) {
				int copies = (int) Math.round(distances.get(i) * 10000 / sum);
				probabilities.addAll(Collections.nCopies(copies, i));
			}
			Collections.shuffle(probabilities);
			centroids[index] = Vectors.normalize(matrix[probabilities.get(rand.nextInt(probabilities.size()))]);
			n++;
		}
	}

}
