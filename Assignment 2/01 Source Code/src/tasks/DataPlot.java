package tasks;

import java.awt.Color;
import java.awt.GraphicsEnvironment;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.labels.XYItemLabelGenerator;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

public class DataPlot {
	
	private double[][] data;
	private double[][] centroids;
	private int[] clusters;
	private JFreeChart chart;
	private String name;
	
	public DataPlot (double[][] data, double[][] centroids, int[] clusters, String name) {
		this.data = data;
		this.centroids = centroids;
		this.clusters = clusters;
		this.name = name;
	}
	
	public DataPlot (double[][] data, String name) {
		this(data, null, null, name);
	}
	
	private class CreatePlot extends JFrame {
	
		private static final long serialVersionUID = -1395188182192417989L;

		private CreatePlot(String title) {
			super(title);
			ChartPanel panel = new ChartPanel(chart);
			setContentPane(panel);
		}
		
	}

	private JFreeChart createChart (String name) {
		XYDataset dataset = createDataset();
		chart = ChartFactory.createScatterPlot(
				"2D Visualization of the " + name, null, null,
				dataset);
		XYItemLabelGenerator labels = new XYItemLabelGenerator() {

			@Override
			public String generateLabel(XYDataset data, int series, int item) {
				if (data.getSeriesCount() == 1)
					return null;
				if (series == data.getSeriesCount() - 1)
					return "Centroid " + (item + 1);
				return null;
			}
			
		};
		XYPlot plot = (XYPlot) chart.getPlot();
		plot.setBackgroundPaint(new Color(192,192,192));
		XYItemRenderer renderer = plot.getRenderer();
		renderer.setBaseItemLabelGenerator(labels);
		renderer.setBaseItemLabelsVisible(true);
		return chart;
	}
	
	private XYDataset createDataset () {
		XYSeriesCollection dataset = new XYSeriesCollection();
		List<XYSeries> series = new ArrayList<>();
		
		if (clusters != null) {
			Arrays.sort(clusters);
			int k = clusters[clusters.length - 1] + 1;
			for (int i = 0; i < k; i++)
				series.add(new XYSeries("Cluster" + (i + 1)));
			for (int i = 0; i < data.length; i++) 
				series.get(clusters[i]).add(data[i][0], data[i][1]);
			for (int i = 0; i < k; i++)
				dataset.addSeries(series.get(i));
			XYSeries centroidsSeries = new XYSeries("Centroids");
			for (int i = 0; i < centroids.length; i++) 
				centroidsSeries.add(centroids[i][0], centroids[i][1]);
			dataset.addSeries(centroidsSeries);
		}
		else {
			XYSeries rawData = new XYSeries("Raw Data");
			for (int i = 0; i < data.length; i++) 
				rawData.add(data[i][0], data[i][1]);
			dataset.addSeries(rawData);
		}

		return dataset;
	}

	public void createPlot () throws IOException {
		chart = createChart(name);
		ChartUtilities.saveChartAsPNG(new File(name + ".png"), chart, 1000, 600);
		System.out.println("A copy of the generated chart has been saved as \"" + name + ".png\"");
		if (!GraphicsEnvironment.isHeadless()) {
			SwingUtilities.invokeLater(() -> {
				CreatePlot plot = new CreatePlot("Scatter Chart");
				plot.setSize(800, 400);
				plot.setLocationRelativeTo(null);
				plot.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
				plot.setVisible(true);				
			});
		} else {
			System.out.println("The generated chart cannot be shown in the current environment.");
		}
	}
}
