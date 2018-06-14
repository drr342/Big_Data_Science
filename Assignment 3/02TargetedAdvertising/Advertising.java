import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

public class Advertising extends Configured implements Tool {
	public static void main(String[] args) throws Exception {
		System.out.println(Arrays.toString(args));
		int res = ToolRunner.run(new Configuration(), new Advertising(), args);

		System.exit(res);
	}

	@SuppressWarnings("deprecation")
	@Override
	public int run(String[] args) throws Exception {
		System.out.println(Arrays.toString(args));
		Job job = new Job(getConf(), "Advertising");
		job.setJarByClass(Advertising.class);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(Text.class);

		job.setMapperClass(Map.class);
		job.setReducerClass(Reduce.class);

		job.setInputFormatClass(TextInputFormat.class);
		job.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));

		job.waitForCompletion(true);

		return 0;
	}

	public static class Map extends Mapper<LongWritable, Text, Text, Text> {
		private Text user = new Text();
		private Text categories = new Text();

		@Override
		public void map(LongWritable key, Text value, Context context)
				throws IOException, InterruptedException {
			String[] token = value.toString().split("\\t");
			user.set(token[0]);
			List<String> catList = Arrays.asList(token[1].split(","));
			for (int i = 1; i <= catList.size(); i++) {
				for (int j = 0; j <= catList.size() - i; j++) {
					categories.set(catList.subList(j, j + i).toString().replaceAll("[\\[\\] ]", ""));
					context.write(categories, user);
				}
			}
		}
	}

	public static class Reduce extends Reducer<Text, Text, Text, Text> {
		@Override
		public void reduce(Text key, Iterable<Text> values, Context context)
				throws IOException, InterruptedException {
			MyMap users = new MyMap();
			for (Text val : values) {
				String sVal = val.toString();
				if (users.containsKey(sVal)) {
					users.put(sVal, users.get(sVal) + 1);
				} else {
					users.put(sVal, 1);
				}
			}
			Text value = new Text();
			value.set(users.toString());
			context.write(key, value);
		}
	}
	
	private static class MyMap extends HashMap<String, Integer> {
		private static final long serialVersionUID = 1L;

		@Override
		public String toString() {
			String out = "";
			for (Entry<String, Integer> entry: this.entrySet()) {
				out += entry.getKey() + "(" + entry.getValue() + ")" + ",";
			}
			out = out.substring(0, out.length() - 1);
			return out;
		}
	}
}
