package net.slycelote.caide.logic;

import com.topcoder.client.contestant.ProblemComponentModel;
import com.topcoder.shared.language.Language;
import com.topcoder.shared.problem.Renderer;

@SuppressWarnings("UnusedDeclaration")
public abstract class BaseEntryPoint {
    /**
     * This method will be called to retrieve the JPanel containing your editor.
     * This method will be called initially (if your plugin is the default plugin) when the coder
     * enters the coding room or called when the coder switches editors.
     * Please note that the contest applet will size the panel accordingly.
     * @return the editor.
     */
    public abstract javax.swing.JPanel getEditorPanel();

    /**
     * This method will be called to retrieve the current source code in your editor.
     * This method will be called when the coder presses the Save/Test/Compile/Submit buttons OR
     * if the coder switches editors or language.
     * @return source code of the solution in the editor.
     */
    public abstract String getSource();

    /**
     * This method will be called to set the current source code.  This method will be called when
     * a problem is opened OR if the coder switches editors or language.
     * Please note that the source could be blank.
     * @param source source code of the solution stored by TC applet.
     */
    public abstract void setSource(String source);

    /**
     * This method will be called from the Editor Preferences window to allow any editor
     * configuration that needs to be preformed. You can store any local preferences by calling the
     * class {@link com.topcoder.client.contestApplet.common.LocalPreferences} – please see the
     * source code for the contest applet for details on the API.
     */
    public void configure() {}

    /**
     * This method is called to clear the current source code from the editor.
     * This method will be called between opening problems to clear the prior problems source code.
     */
    public void clear() {}

    /**
     * This method is called to enable/disable editing of text within the editor.
     * This method will be called with TRUE when you enter (or reenter) the coding phase
     * and will be called with FALSE when the coding phase ends.
     * @param enable {@code true}, if code editing gets enabled; {@code false} otherwise.
     */
    public void setTextEnabled(Boolean enable) {}

    /**
     * This method gives you all the information about the problem, the current language and
     * the specific component of the problem being edited (there may be more than one in a team environment).
     * Please refer to the javadoc on the {@link com.topcoder.shared.problem.Problem}
     * and {@link com.topcoder.shared.language.Language} objects for details.
     * You must import {@link com.topcoder.client.contestant.ProblemComponentModel},
     * {@link com.topcoder.shared.problem.Renderer} and {@link com.topcoder.shared.language}
     * from the ContestApplet.jar file.
     * @param component information about the problem
     * @param language selected programming language
     * @param renderer interface for rendering
     */
    public void setProblemComponent(ProblemComponentModel component, Language language, Renderer renderer) {}

    /**
     * Tells the plugin the UNIQUE name that was given to it.
     * This is useful to the plugin to denote each unique instance of the plugin that the user has
     * setup (so that plugin configuration information can be kept unique per instance).
     * @param name the unique plugin name
     */
    public void setName(String name) {}

    /**
     * This method is called ONCE (per instance) when the plugin is being installed.
     * This gives the plugin a chance to setup (or request) the plugin when it is installed.
     * The plugin can throw an runtime exception to prevent the installation of the plugin if
     * a critical error occurs.
     */
    public void install() {}

    /**
     * This method is called ONCE (per instance) when the plugin is being uninstalled.
     * This gives the plugin a chance to cleanup any resources that were used.
     */
    public void uninstall() {}

    /**
     * Tells the plugin that the plugin is about to be used.
     */
    public void startUsing() {}

    /**
     * Tells the plugin that the plugin will not be used until the next startUsing() method call.
     */
    public void stopUsing() {}

    /**
     * Tells the plugin that the plugin object will no longer be called or referenced
     * (ie can be garbage collected).
     */
    public void dispose() {}

    /**
     * Tells the applet that the plugin object can be cache’d and reused or not.
     * The applet should return TRUE (the default) if the plugin object can be reused.
     * Return FALSE if the a new plugin object should be instantiated each time it is needed.
     * @return {@code true}, if the plugin object can be reused
     */
    public Boolean isCacheable() {return true;}

    //public Boolean setCompileResults(Boolean success, String message) {}
}

