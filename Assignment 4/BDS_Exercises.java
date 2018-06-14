package org.deeplearning4j.examples.nlp.word2vec;

import org.deeplearning4j.api.storage.StatsStorage;
import org.deeplearning4j.models.embeddings.WeightLookupTable;
import org.deeplearning4j.models.embeddings.loader.WordVectorSerializer;
import org.deeplearning4j.models.word2vec.Word2Vec;
import org.deeplearning4j.models.word2vec.wordstore.VocabCache;
import org.deeplearning4j.plot.BarnesHutTsne;
import org.deeplearning4j.ui.api.UIServer;
import org.deeplearning4j.ui.storage.InMemoryStatsStorage;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.factory.Nd4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;

public class BDS_Exercises {

    private static Logger log = LoggerFactory.getLogger(BDS_Training.class);
    private static Word2Vec vec = WordVectorSerializer.readWord2VecModel("word2vec_model.txt");

    private static void closest () {
        // Prints out the closest 10 words.
        String[] words =
            {"cat", "animal", "science", "scientific", "vector", "vendor", "car",
                "hot", "major", "man", "doctor", "flower", "capital", "washington"};
        log.info("Closest Words:");
        Collection<String> lst;
        for (String word : words) {
            try {
                lst = vec.wordsNearestSum(word, 10);
                log.info("10 Words closest to {}: {}", word, lst);
                lst.clear();
            } catch (Exception e) {
                log.info("The word {} was not found in the corpus!", word);
            }
        }
    }

    private static void analogies () {
        // Print out the closest 5 analogies.
        String[][] positive = {{"king", "woman"}, {"paris", "italy"}, {"car", "birds"},
            {"man", "nurse"}, {"fall", "live"}, {"important", "unwilling"}};
        String[] negative = {"queen", "france", "cars", "woman", "stand", "unimportant"};
        log.info("Closest Analogies:");
        Collection<String> aList;
        for (int i = 0; i < negative.length; i++) {
            aList = vec.wordsNearest(Arrays.asList(positive[i]), Arrays.asList(negative[i]), 5);
            log.info("5 Words that best complete the analogy: {}:{}::{}:{}",
                positive[i][0], negative[i], aList, positive[i][1]);
            aList.clear();
        }
    }

    private static void visualization () throws IOException {
        log.info("Model 2D Visualization:");
        // Get model weights
        WeightLookupTable weightLookupTable = vec.lookupTable();
        // Get model vocabulary
        VocabCache vocabCache = weightLookupTable.getVocabCache();
        // Get set of known vocabulary words
        Set<String> known = new HashSet<>();
        for (Object s: vocabCache.words()) {
            known.add(s.toString());
        }

        List<String> words = new ArrayList<>();
        // Read external word list
        Scanner s = new Scanner(new File("simlex999_uniq_sort.txt"));
        while (s.hasNext()){
            String tok = s.next();
            if (known.contains(tok)) {
                words.add(tok);
            }
        }
        s.close();

        double[][] doubleMatrix = new double[words.size()][];
        for (int i=0; i<words.size(); i++) {
            doubleMatrix[i] = vec.getWordVector(words.get(i));
        }
        INDArray matrix = Nd4j.create(doubleMatrix);

        BarnesHutTsne tsne = new BarnesHutTsne.Builder()
            .theta(0.0)
            .build();
        tsne.fit(matrix);
        tsne.saveAsFile(words, "tsne.csv");
        UIServer uiServer = UIServer.getInstance();
        StatsStorage statsStorage = new InMemoryStatsStorage();
        uiServer.attach(statsStorage);
    }

    public static void main (String args[]) throws IOException {
        System.out.println();
        closest();
        System.out.println();
        analogies();
        System.out.println();
        visualization();
    }

}
