package test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.Collectors;

import tasks.DataPlot;
import tasks.DocumentTermMatrix;
import tasks.FileLoader;
import tasks.Kmeans;
import tasks.Kmeanspp;
import tasks.Preprocessor;
import tasks.SVD;
import utils.Clustering;
import utils.Cosine;
import utils.Euclidean;
import utils.Similarity;
import utils.Vectors;

public class Tester {
	
	private static int k;
	private static int[] clusters;
	private static List<String> filesNames;
	private static List<List<String>> keyWords = new ArrayList<>();
		
	private static void printClusterInfo (int k) {
		String files = "";
		Set<String> clusterKeywords = new HashSet<>();
		Set<String> auxiliarySet = new HashSet<>();
		System.out.println("Cluster " + (k + 1) + ":");
		for (int i = 0; i < clusters.length; i++) {
			if (k == clusters[i]) {
				files = files + "\t\t" + filesNames.get(i) + "\n";
				for (String word: keyWords.get(i)) {
					if(!auxiliarySet.add(word))
						clusterKeywords.add(word);
				}
			}
		}
		System.out.println("\tKeywords associated to the cluster:");
		System.out.println("\t\t" + clusterKeywords);
		System.out.println("\tFiles in the cluster:");
		System.out.print(files);
	}

	public static void main(String[] args) throws IOException {
		Scanner sc = new Scanner(System.in);
		String answer = "";
		while (!answer.matches("[YyNn]|[Yy][Ee][Ss]|[Nn][Oo]")) {
			System.out.print("Use preprocessed documents included in JAR file? (y/n) ");
			answer = sc.nextLine();
		}
		String algorithm = "";
		while (!algorithm.matches("[01]")) {
			System.out.print("Clustering algorithm: (0 = K-means, 1 = K-means++) ");
			algorithm = sc.nextLine();
		}
		String similarity = "";
		while (!similarity.matches("[01]")) {
			System.out.print("Similarity measure for the clustering algorithm: (0 = Euclidean, 1 = Cosine) ");
			similarity = sc.nextLine();
		}
		String kString = "";
		while (true) {
			System.out.print("Number of clusters k (at least 2): ");
			kString = sc.nextLine();
			if (!kString.matches("\\d+")) continue;
			k = Integer.parseInt(kString);
			if (k >= 2) break;
		}
		List<List<String>> documents;
		if (answer.toLowerCase().charAt(0) == 'y') {
			documents = FileLoader.loadPreprocessedDocuments();
			filesNames = FileLoader.loadPreprocessedFiles();
		} else {
			System.out.println("Path to the directory with the documents to analyze:");
			String path = sc.nextLine();
			List<File> files = FileLoader.getFiles(path);
			filesNames = files.stream().map(File::toString).collect(Collectors.toList());
			documents = Preprocessor.cleanDocuments(files);
		}
		sc.close();
		
		System.out.println();
		System.out.println("========================================================");
		System.out.println("PREPROCESSING RESULTS");
		System.out.println("========================================================");
		System.out.println("Top 10 keywords in each document according to the TF-IDF matrix:");
		DocumentTermMatrix dtm = new DocumentTermMatrix(documents);
		for (int i = 0; i < filesNames.size(); i++) {
			List<String> kw = dtm.getKeyWords(i, 10);
			keyWords.add(kw);
			System.out.println("\t" + filesNames.get(i) + "\t" + kw);
		}
		
		System.out.println();
		System.out.println("========================================================");
		System.out.println("CLUSTERING RESULTS");
		System.out.println("========================================================");
		double[][] matrix = dtm.getTfidfMatrix();
		double[][] normalizedMatrix = new double[matrix.length][];
		for (int i = 0; i < matrix.length; i++) {
			normalizedMatrix[i] = Vectors.normalize(matrix[i]);
		}
		Similarity measure = similarity.equals("0") ? new Euclidean() : new Cosine();
		Clustering model = algorithm.equals("0") ? 
				new Kmeans(normalizedMatrix, measure, k) : new Kmeanspp(normalizedMatrix, measure, k);
		clusters = model.getClusters();
		for (int i = 0; i < k; i++) {
			printClusterInfo(i);
		}
				
		System.out.println();
		System.out.println("========================================================");
		System.out.println("VISUALIZATION RESULTS");
		System.out.println("========================================================");
		double[][] centroids = model.getCentroids();
		double[][] normalizedCentroids = new double[centroids.length][];
		for (int i = 0; i < centroids.length; i++) {
			normalizedCentroids[i] = Vectors.normalize(centroids[i]);
		}
		double[][] reducedMatrix = SVD.reduce(normalizedMatrix, 2);
		double[][] reducedCentroids = SVD.reduce(normalizedCentroids, 2);
		DataPlot rawPlot = new DataPlot(reducedMatrix, "Raw Data");
		DataPlot clustersPlot = new DataPlot(reducedMatrix, reducedCentroids, clusters, "Clustered Data");
		rawPlot.createPlot();
		clustersPlot.createPlot();
	}

}
