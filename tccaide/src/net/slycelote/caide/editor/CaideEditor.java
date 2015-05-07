package net.slycelote.caide.editor;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;

public class CaideEditor extends JPanel {
    private JTextPane logPane = new JTextPane();

    public CaideEditor() {
        super(new BorderLayout());
        logPane = new JTextPane();
        logPane.setContentType("text/html");
        logPane.setBackground(Color.black);
        logPane.setEditable(false);

        JScrollPane scrollPane = new JScrollPane(logPane);
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        add(scrollPane, BorderLayout.CENTER);
        repaint();
    }

    public void logError(final String message) {
        addMessage(message, errorAttributes);
    }

    public void logException(final Exception e) {
        logError(e.getMessage());
    }

    public void logMessage(final String message) {
        addMessage(message, messageAttributes);
    }

    private void addMessage(String message, AttributeSet attributes) {
        StyledDocument doc = logPane.getStyledDocument();
        try {
            doc.insertString(doc.getLength(), message + "\n", attributes);
        } catch (BadLocationException ignored) {
        }
        logPane.setEditable(false);
    }

    private static final MutableAttributeSet errorAttributes = new SimpleAttributeSet();
    private static final MutableAttributeSet messageAttributes = new SimpleAttributeSet();

    static {
        StyleConstants.setForeground(errorAttributes, Color.red);
        StyleConstants.setBackground(errorAttributes, Color.black);
        errorAttributes.addAttribute("readonly", true);

        StyleConstants.setForeground(messageAttributes, Color.white);
        StyleConstants.setBackground(messageAttributes, Color.black);
        messageAttributes.addAttribute("readonly", true);
    }
}
