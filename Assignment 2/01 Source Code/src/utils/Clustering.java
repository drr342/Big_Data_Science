package utils;

import java.util.Arrays;

public abstract class Clustering {
	
	protected double[][] matrix;
	protected Similarity measure;
	protected int k;
	protected double[][] centroids;
	protected int[] clusters;
	
	public Clustering (double[][] matrix, Similarity measure, int k) {
		this.matrix = matrix;
		this.measure = measure;
		this.k = k;
		this.centroids = new double[k][matrix[0].length];
		this.clusters = new int[matrix.length];
		run();
	}
	
	private void run () {
		Arrays.fill(clusters, 0);
		initializeCentroids();
		boolean flag = true;
		while (flag) {
			flag = updateClusters();
			updateCentroids();
		}
	}
	
	protected abstract void initializeCentroids ();
	
	private boolean updateClusters () {
		boolean flag = false;
		for (int i = 0; i < matrix.length; i++) {
			double[] boundary = {Double.NaN, -1};
			for (int j = 0; j < k; j++) {
				boundary = updateBoundary(boundary, i, j);
			}
			if (clusters[i] != (int) boundary[1]) {
				clusters[i] = (int) boundary[1];
				flag = true;
			}
		}
		return flag;
	}
	
	private double[] updateBoundary (double[] boundary, int i, int j) {		
		double[] newBoundary = {measure.distance(matrix[i], centroids[j]), j};
		if (!Double.isNaN(boundary[0])) {
			if (measure instanceof Euclidean && boundary[0] < newBoundary[0])
				newBoundary = boundary;
			else if (measure instanceof Cosine && boundary[0] > newBoundary[0])
				newBoundary = boundary;
		}
		return newBoundary;
	}
	
	private void updateCentroids () {
		for (int j = 0; j < k; j++) {
			double[] sum = new double[matrix[0].length];
			Arrays.fill(sum, 0.0);
			int count = 0;
			for (int i = 0; i < matrix.length; i++) {
				if (clusters[i] == j) {
					sum = Vectors.sumArrays(sum, matrix[i]);
					count++;
				}
			}
			if (measure instanceof Euclidean)
				centroids[j] = Vectors.average(sum, count);
			else
				centroids[j] = Vectors.average(sum, Vectors.norm(sum));
		}
	}
	
	public int[] getClusters () {
		return clusters;
	}
	
	public double[][] getCentroids () {
		return centroids;
	}

}
