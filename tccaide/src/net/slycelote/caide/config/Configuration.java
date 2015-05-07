package net.slycelote.caide.config;

import com.topcoder.client.contestApplet.common.LocalPreferences;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Configuration {
    private static LocalPreferences preferences = LocalPreferences.getInstance();
    private static final String CAIDE_PROJECT_DIR_KEY = "net.slycelote.caide.project";

    public static String getCaideProjectDir() {
        return preferences.getProperty(CAIDE_PROJECT_DIR_KEY);
    }
    public static void setCaideProjectDir(String dir) {
        preferences.setProperty(CAIDE_PROJECT_DIR_KEY, dir);
    }

    public static String getCaideExe() throws IOException {
        Path caideExeFile = Paths.get(getCaideProjectDir(), ".caide", "caideExe.txt");
        return new String(Files.readAllBytes(caideExeFile));
    }
}
