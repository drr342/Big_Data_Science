package tasks;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import edu.stanford.nlp.simple.Document;
import edu.stanford.nlp.simple.Sentence;


public final class Preprocessor {
	
	private static List<String> stopWords = new ArrayList<>();
	private static List<File> files = new ArrayList<>();
	
	private static final int NGRAM_THRESHOLD = 4; 
	
	static {
		try {
			loadStopWords();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private Preprocessor () {
		throw new RuntimeException();
	}
	
	private static void loadStopWords() throws IOException {
		InputStream stream = Preprocessor.class.getClassLoader().getResourceAsStream("stopwords.txt");
		BufferedReader br = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
		String word;
		while((word = br.readLine()) != null) {
			stopWords.add(word);
		}
		br.close();
	}
	
	private static List<List<Sentence>> filterStopWords_lemmatization () throws IOException {
		List<List<Sentence>> documents = new ArrayList<>();
		for (File f: files) {
			BufferedReader br = new BufferedReader(new FileReader(f));
			String line;
			String article = "";
			while((line = br.readLine()) != null) {
				article = article + " " + (line.isEmpty() ? ". " : line);
			}
			br.close();
			Document d = new Document(article);
			List<Sentence> document = new ArrayList<>();
			for (Sentence sentence: d.sentences()) {
				List<String> words = new ArrayList<>();
				words.addAll(Collections.nCopies(sentence.lemmas().size(), null));
				Collections.copy(words, sentence.lemmas());
				int i = 0;
				while (i < words.size()) {
					if (stopWords.contains(words.get(i).toLowerCase()))
						words.remove(i);
					else
						i++;
				}
				if (!words.isEmpty())
					document.add(new Sentence(words));
			}
			documents.add(document);
		}
		return documents;
	}
	
	private static List<List<Sentence>> applyNER () throws IOException {
		List<List<Sentence>> documentsBefore = filterStopWords_lemmatization();
		List<List<Sentence>> documentsAfter = new ArrayList<>();
		int totalD = documentsBefore.size();
		int currentD = 0;
		for (List<Sentence> documentBefore: documentsBefore) {
			currentD++;
			List<Sentence> documentAfter = new ArrayList<>();
			int totalS = documentBefore.size();
			int currentS = 0;
			for (Sentence sentenceBefore: documentBefore) {
				currentS++;
				List<String> nerTags = sentenceBefore.nerTags();
				List<String> nerWords = new ArrayList<>();
				System.out.format("Preprocessing document %d of %d: %.2f%% complete...\n", 
						currentD, totalD, (currentS * 100.0) / totalS);
				for (int j = 0; j < nerTags.size(); j++) {
					String nerWord = sentenceBefore.word(j);
					int k = j + 1;
					switch (nerTags.get(j)) {
						case "TITLE":
						case "PERSON":
							while (k <  nerTags.size() && nerTags.get(k).equals("PERSON"))
								nerWord = nerWord + "_" + sentenceBefore.word(k++);
							break;
						case "ORGANIZATION":
							while (k <  nerTags.size() && nerTags.get(k).equals("ORGANIZATION"))
								nerWord = nerWord + "_" + sentenceBefore.word(k++);
							break;
						case "LOCATION":
							while (k <  nerTags.size() && nerTags.get(k).equals("LOCATION"))
								nerWord = nerWord + "_" + sentenceBefore.word(k++);
							break;
						case "CITY":
							while (k <  nerTags.size() && nerTags.get(k).equals("CITY"))
								nerWord = nerWord + "_" + sentenceBefore.word(k++);
							break;
						case "COUNTRY":
							while (k <  nerTags.size() && nerTags.get(k).equals("COUNTRY"))
								nerWord = nerWord + "_" + sentenceBefore.word(k++);
							break;
						default:
							break;
					}
					nerWords.add(nerWord);
					j = k - 1;
				}
				Sentence sentenceAfter = new Sentence(nerWords);
				documentAfter.add(sentenceAfter);
			}
			documentsAfter.add(documentAfter);
		}
		return documentsAfter;
	}
	
	private static List<List<String>> mergeNgrams () throws IOException {
		List<List<Sentence>> documents = applyNER();
		List<List<String>> bagsOfWords = new ArrayList<>();
		for (List<Sentence> document: documents) {
			List<String> bagOfWords = new ArrayList<>();
			for (Sentence sentence: document) {
				bagOfWords.addAll(sentence.words());
			}
			bagsOfWords.add(bagOfWords);
		}
		List<String> vocabulary = new ArrayList<>();
		for (List<String> bagOfWords : bagsOfWords) {
			vocabulary.addAll(bagOfWords);
		}
		List<Ngram> bigrams = countNgrams(vocabulary, 2, NGRAM_THRESHOLD);
		List<Ngram> trigrams = countNgrams(vocabulary, 3, NGRAM_THRESHOLD);
		List<Ngram> fourgrams = countNgrams(vocabulary, 4, NGRAM_THRESHOLD);
		
		List<List<String>> newBagsOfWords = new ArrayList<>();
		for (List<String> bagOfWords : bagsOfWords) {
			String s = bagOfWords.toString().replaceAll("[\\[\\]\\,]", "");
			for (Ngram fourgram : fourgrams)
				s = s.replaceAll(fourgram.toString(), fourgram.toNgram());
			for (Ngram trigram : trigrams)
				s = s.replaceAll(trigram.toString(), trigram.toNgram());
			for (Ngram bigram : bigrams)
				s = s.replaceAll(bigram.toString(), bigram.toNgram());
			List<String> newBagOfWords = Arrays.asList(s.split(" "));
			newBagOfWords = newBagOfWords.stream().map(String::toLowerCase).collect(Collectors.toList());
			newBagsOfWords.add(newBagOfWords);
		}
		return newBagsOfWords;
	}
	
	private static class Ngram {
		public List<String> words = new ArrayList<>();
		
		public Ngram (List<String> words) {
			this.words.addAll(words);
		}
		
		@Override
		public boolean equals (Object o) {
			if (!(o instanceof Ngram))
				return false;
			Ngram other = (Ngram) o;
			return this.words.equals(other.words);
		}
		
		@Override
		public String toString () {
			return words.toString().replaceAll("[\\[\\]\\,]", "");
		}
		
		public String toNgram () {
			return this.toString().replaceAll(" ", "_");
		}
		
		@Override
		public int hashCode () {
			return words.hashCode();
		}
	}
	
	private static List<Ngram> countNgrams (List<String> vocabulary, int n, int threshold) {
		Map<Ngram, Integer> counts = new HashMap<>();
		for (int i = n; i <= vocabulary.size(); i++) {
			Ngram ngram = new Ngram(vocabulary.subList(i - n, i));
			if (!counts.containsKey(ngram))
				counts.put(ngram, 1);
			else 
				counts.put(ngram, counts.get(ngram) + 1);
		}
		List<Ngram> ngrams = new ArrayList<>();
		for (Entry<Ngram, Integer> entry : counts.entrySet()) {
			if (entry.getValue() >= threshold)
				ngrams.add(entry.getKey());
		}
		return ngrams;
	}
	
	public static List<List<String>> cleanDocuments (List<File> documents) throws IOException {
		files = documents;
		return mergeNgrams();
	}

}
