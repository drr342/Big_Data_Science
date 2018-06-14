package tasks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DocumentTermMatrix {
	
	private int docsN, termsN;
	private List<List<String>> documents;
	private List<String> terms = new ArrayList<>();
	private int[][] countMatrix;
	private double[][] tfidfMatrix;
	
	public DocumentTermMatrix (List<List<String>> documents) {
		this.documents = documents;
		docsN = documents.size();
		Set<String> setTerms = new HashSet<>();
		for (List<String> document : documents) {
			setTerms.addAll(document);
		}
		terms.addAll(setTerms);
//		Collections.sort(terms);
		termsN = terms.size();
		countMatrix = new int[docsN][termsN];
		for (int i = 0; i < docsN; i++) {
			List<String> document = documents.get(i);
			for (String term : document) {
				countMatrix[i][terms.indexOf(term)]++;
			}
		}
		tfidfMatrix = new double[docsN][termsN];
		for (int i = 0; i < docsN; i++) {
			for (int j = 0; j < termsN; j++) {
				tfidfMatrix[i][j] = tf(i, j) * idf(j);
			}
		}
	}
	
	private double tf (int docID, int termID) {
		return (double) countMatrix[docID][termID] / documents.get(docID).size();
	}
	
	private double idf (int termID) {
		int count = 0;
		for (List<String> document : documents) {
			if (document.contains(terms.get(termID))) count++;
		}
		return Math.log((double) documents.size() / count);
	}
	
	public double getValue(int i, int j) {
		if (i >= docsN || j >= termsN)
			return Double.NaN;
		return tfidfMatrix[i][j];
	}
	
	public double[][] getTfidfMatrix() {
		return tfidfMatrix;
	}
	
	public List<String> getKeyWords (int docID, int n) {
		if (docID >= documents.size())
			return null;
		List<Tuple> values = new ArrayList<>();
		List<String> document = documents.get(docID);
		for (int j = 0; j < document.size(); j++) {
			int termID = terms.indexOf(document.get(j));
			Tuple tuple = new Tuple(termID, getValue(docID, termID));
			if (!values.contains(tuple))
				values.add(tuple);
		}
		Collections.sort(values, Collections.reverseOrder());
		List<String> keyWords = new ArrayList<>();
		for (Tuple tuple : values.subList(0, n)) {
			keyWords.add(terms.get(tuple.index));
		}
		return keyWords;
	}
	
	private static class Tuple implements Comparable<Tuple> {
		
		private int index;
		private double value; 
		
		private Tuple (int index, double value) {
			this.index = index;
			this.value = value;
		}

		@Override
		public int compareTo (Tuple other) {
			if (this.value != other.value)
				return Double.compare(this.value, other.value);
			return Integer.compare(this.index, other.index);
		}
		
		@Override
		public boolean equals (Object o) {
			if (!(o instanceof Tuple))
				return false;
			Tuple other = (Tuple) o;
			return this.index == other.index && this.value == other.value;
		}
		
	}

}
