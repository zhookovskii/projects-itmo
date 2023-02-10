package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;

public class Md2Html {
    public static void main(String[] args) {
        StringBuilder res = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                new FileInputStream(args[0]), StandardCharsets.UTF_8))) {
            String s = "";
            StringBuilder text = new StringBuilder();
            while ((s != null) && (s = reader.readLine()) != null) {
                while (s!= null && !s.equals("")) {
                    text.append(s).append('\n');
                    s = reader.readLine();
                }
                if (text.length() != 0) {
                    text.setLength(text.length() - 1);
                    new Parse(text).toHtml(res);
                    res.append('\n');
                    text.setLength(0);
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("Input file not found " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Something went wrong " + e.getMessage());
        }
        try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
            writer.write(res.toString());
        } catch (FileNotFoundException e) {
            System.err.println("Output file not found " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Something went wrong " + e.getMessage());
        }
    }
}
