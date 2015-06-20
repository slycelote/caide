package net.slycelote.caide.logic;

import net.slycelote.caide.config.Configuration;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CaideExe {
    public static class ExecutionResult {
        public final String output;
        public final int exitCode;

        public ExecutionResult(String output, int exitCode) {
            this.output = output;
            this.exitCode = exitCode;
        }
    }

    public static ExecutionResult run(final String... args) {
        try {
            String caideExe = Configuration.getCaideExe();
            List<String> command = new ArrayList<String>();
            command.add(caideExe);
            Collections.addAll(command, args);
            final Process process = new ProcessBuilder(command)
                    .redirectErrorStream(true)
                    .directory(new File(Configuration.getCaideProjectDir()))
                    .start();
            OutputStream stdin = process.getOutputStream();
            stdin.close();
            final InputStream stdout = process.getInputStream();
            final StringBuilder sb = new StringBuilder();
            final Thread reader = new Thread(new Runnable() {
                @Override
                public void run() {
                    BufferedReader stdoutReader = null;
                    try
                    {
                        stdoutReader = new BufferedReader(new InputStreamReader(stdout));
                        String line;
                        while ((line = stdoutReader.readLine()) != null) {
                            sb.append(line);
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    finally {
                        if (stdoutReader != null) {
                            try {
                                stdoutReader.close();
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        }
                    }
                }
            });
            reader.start();

            process.waitFor();
            return new ExecutionResult(sb.toString(), process.exitValue());
        } catch (IOException e) {
            return new ExecutionResult(e.getMessage(), 11111);
        } catch (InterruptedException e) {
            return new ExecutionResult(e.getMessage(), 11112);
        }
    }
}
