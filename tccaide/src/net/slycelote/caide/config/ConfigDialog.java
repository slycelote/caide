package net.slycelote.caide.config;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

// Mostly copied from Greed plugin: https://github.com/shivawu/topcoder-greed
public class ConfigDialog extends JDialog implements ActionListener {
    private JButton saveButton;
    private JButton cancelButton;

    private JTextField caideFolder;

    public ConfigDialog() {
        super((JFrame) null, "Caide configuration", true);
        setSize(new Dimension(200, 100));
        super.setLocationRelativeTo(getParent());

        Container contentPane = getContentPane();
        contentPane.setLayout(new FlowLayout());

        JLabel label = new JLabel("Caide root folder: ");
        label.setForeground(Color.WHITE);
        contentPane.add(label);

        caideFolder = new JTextField("", 35);
        caideFolder.setBackground(Color.BLACK);
        caideFolder.setForeground(Color.WHITE);
        caideFolder.setMinimumSize(new Dimension(100, 30));
        caideFolder.setText(Configuration.getCaideProjectDir());
        contentPane.add(caideFolder);

        saveButton = new JButton("Verify & Save");
        contentPane.add(saveButton);

        cancelButton = new JButton("Cancel");
        contentPane.add(cancelButton);

        saveButton.addActionListener(this);
        cancelButton.addActionListener(this);

        pack();
    }

    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        Object src = actionEvent.getSource();
        if (src == saveButton) {
            String folder = caideFolder.getText();
            if (checkAndSave(folder))
                this.dispose();
        } else if (src == cancelButton) {
            this.dispose();
        }
    }

    private boolean checkAndSave(String caideProject) {
        if ("".equals(caideProject)) {
            showMessageBox("You must specify a folder");
            return false;
        } else if (!new File(caideProject).exists() || !new File(caideProject).isDirectory()) {
            showMessageBox("Invalid folder! Maybe a non-existent path, or a regular file with the same name exists?");
            return false;
        }

        Configuration.setCaideProjectDir(caideProject);
        return true;
    }

    private void showMessageBox(String message) {
        JOptionPane.showConfirmDialog(this, message, "Warning", JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE);
    }

    public static void main(String[] args) {
        new ConfigDialog().setVisible(true);
    }
}
