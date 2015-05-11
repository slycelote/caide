package net.slycelote.caide;

import com.topcoder.client.contestant.ProblemComponentModel;
import com.topcoder.shared.language.CPPLanguage;
import com.topcoder.shared.language.CSharpLanguage;
import com.topcoder.shared.language.Language;
import com.topcoder.shared.problem.DataType;
import com.topcoder.shared.problem.Renderer;
import com.topcoder.shared.problem.TestCase;
import net.slycelote.caide.config.ConfigDialog;
import net.slycelote.caide.config.Configuration;
import net.slycelote.caide.editor.CaideEditor;
import net.slycelote.caide.logic.BaseEntryPoint;
import net.slycelote.caide.logic.CaideExe;
import net.slycelote.caide.model.Lang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


public class EntryPoint extends BaseEntryPoint {
    public javax.swing.JPanel getEditorPanel() {
        return editor;
    }

    public String getSource() {
        String root = Configuration.getCaideProjectDir();
        Path solutionFile = Paths.get(root, problemName, "submission." + language.extension);
        try {
            return new String(Files.readAllBytes(solutionFile));
        } catch (IOException e) {
            editor.logError(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    public void setSource(String source) {
        if (isDebugMode()) {
            editor.logError("Setting source code is not supported");
        }
    }

    private void debug(ProblemComponentModel component) {
    }

    @Override
    public void setProblemComponent(ProblemComponentModel component, Language language, Renderer renderer) {
        debug(component);
        problemName = component.getClassName();

        if (language.getId() == CPPLanguage.CPP_LANGUAGE.getId())
            this.language = Lang.CPP;
//        else if (language.getId() == CSharpLanguage.CSHARP_LANGUAGE.getId())
//            this.language = Lang.CSHARP;
        else {
            this.language = Lang.UNSUPPORTED;
            editor.logError(language.getName() + " is not supported.");
            return;
        }

        final String root = Configuration.getCaideProjectDir();
        final String testMethodDesc = getTestMethodDescriptor(component);

        CaideExe.ExecutionResult ret;
        if (Files.exists(Paths.get(root, problemName, "problem.ini"))) {
            ret = CaideExe.run("checkout", problemName, "--lang", this.language.name);
        } else {
            ret = CaideExe.run("problem", problemName, "--type", "topcoder," + testMethodDesc,
                    "--lang", this.language.name);
        }

        if (ret.exitCode != 0) {
            editor.logError(ret.output);
            return;
        }

        editor.logMessage(ret.output);

        saveTestCases(component.getTestCases(), root);
    }

    private String getTestMethodDescriptor(ProblemComponentModel component) {
        StringBuilder sb = new StringBuilder();
        sb
            .append(component.getClassName())
            .append(',')
            .append(component.getMethodName())
            .append(':')
            .append(typeToString(component.getReturnType()));

        String[] paramNames = component.getParamNames();
        for (int i = 0; i < paramNames.length; ++i) {
            sb
                .append(',')
                .append(paramNames[i])
                .append(':')
                .append(typeToString(component.getParamTypes()[i]));
        }

        return sb.toString();
    }

    private String typeToString(DataType dataType) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < dataType.getDimension(); ++i) {
            sb.append('v');
        }
        sb.append(dataType.getBaseName());
        return sb.toString();
    }

    private void saveTestCases(TestCase[] testCases, String root) {
        for (int i = 0; i < testCases.length; ++i) {
            TestCase testCase = testCases[i];

            Path fileName = Paths.get(root, problemName, "case" + (i + 1) + ".in");
            if (!Files.exists(fileName)) {
                List<String> input = new ArrayList<String>();
                Collections.addAll(input, testCase.getInput());
                try {
                    Files.write(fileName, input);
                } catch (IOException e) {
                    editor.logException(e);
                }
            }

            fileName = Paths.get(root, problemName, "case" + (i + 1) + ".out");
            if (!Files.exists(fileName)) {
                List<String> output = new ArrayList<String>();
                output.add(testCase.getOutput());
                try {
                    Files.write(fileName, output);
                } catch (IOException e) {
                    editor.logException(e);
                }
            }
        }
    }

    @Override
    public void configure() {
        new ConfigDialog().setVisible(true);
    }

    @Override
    public Boolean isCacheable() {
        return !isDebugMode();
    }

    protected boolean isDebugMode() {
        return false;
    }

    private CaideEditor editor = new CaideEditor();
    private String problemName;
    private Lang language;

    public static void main(String[] args) {
        new EntryPoint();
    }
}
