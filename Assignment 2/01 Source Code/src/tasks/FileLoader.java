package tasks;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class FileLoader {
	
	private static List<File> files = new ArrayList<>();
	
	private FileLoader () {
		throw new RuntimeException();
	}
	
	private static void populateFilesList (File file) {
		if (!file.isDirectory()) {
			if (file.toString().endsWith(".txt"))
				files.add(file);
			return;
		}
		for (File subfile: file.listFiles()) {
			populateFilesList(subfile);
		}
	}
	
	public static List<File> getFiles (String path) {
		File root = new File(path);
		files.clear();
		populateFilesList(root);
		System.out.format("%d text files were found.\n", files.size());
		return files;
	}
	
	public static List<List<String>> loadPreprocessedDocuments () throws IOException {
		InputStream stream = FileLoader.class.getClassLoader().getResourceAsStream("preprocessed.txt");
		BufferedReader br = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
//		File f = new File(System.getProperty("user.dir") + "\\files\\preprocessed.txt");
//		BufferedReader br = new BufferedReader(new FileReader(f));
		String document;
		List<List<String>> documents = new ArrayList<>();
		while((document = br.readLine()) != null) {
			document = document.replaceAll("[\\[\\]\\,]", "");
			documents.add(Arrays.asList(document.split(" ")));
		}
		br.close();
		return documents;
	}
	
	public static List<String> loadPreprocessedFiles () {
		String[] folders = {"C1", "C4", "C7"};
		List<String> names = new ArrayList<>();
		for (int i = 0; i < 24; i++) {
			String name = folders[i / 8] + "/article0" + ((i % 8) + 1) + ".txt";
			names.add(name);
		}
		return names;
	}

}
